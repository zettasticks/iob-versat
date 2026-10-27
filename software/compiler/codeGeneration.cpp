#include "codeGeneration.hpp"

#include "templateEngine.hpp"
#include "globals.hpp"
#include "codeGeneration_meta.hpp"
#include "addressGen.hpp"

#include "CEmitter.hpp"

// nocheckin: Move to utils and change name to OS something
#include <filesystem>
namespace fs = std::filesystem;
String GetRelativePathFromSourceToTarget(String sourcePath,String targetPath,Arena* out){
  TEMP_REGION(temp,out);
  fs::path source(StaticFormat("%.*s",UN(sourcePath)));
  fs::path target(StaticFormat("%.*s",UN(targetPath)));

  fs::path res = fs::relative(target,source);

  return PushString(out,"%s",res.c_str());
}

// ======================================
// Code generation

struct FormatContent{
  bool isText;

  union{
    String text;
    int index;
  };
};

Array<FormatContent> ParseFormat(String format,Arena* out){
  TEMP_REGION(temp,out);

  int size = format.size;
  if(size <= 0){
    return {};
  }

  auto list = PushList<FormatContent>(temp);

  int STATE_PARSING_TEXT = 0;
  int STATE_PARSING_INDEX = 1;

  int index = 0;
  int state = (format[0] == '{' ? STATE_PARSING_INDEX : STATE_PARSING_TEXT);
  while(index < size){
    switch(state){
      case 0:{
        int start = index;
        while(index < size && format[index] != '{'){
          index += 1;
        }

        FormatContent* form = list->PushElem();
        form->isText = true;
        form->text = String(&format[start],index - start);
      } break;
      case 1:{
        Assert(format[index] == '{');
        // Only supporting 10 numbers but should be enough. 
        // Do not use format enough to warrant more complexity for now.
        char number = format[index + 1];
        Assert(format[index + 2] == '}');
        
        index += 3;

        FormatContent* form = list->PushElem();
        form->isText = false;
        form->index = number - '0';
      } break;
    }
    
    state = 1 - state;
  }

  return PushArray(out,list);
}

// ======================================
// Type

bool GEN_IsConfigOnly(GEN_WireType type){
  bool res = (type == GEN_WireType_NONE);
  return res;
}

// ======================================
// Naming schemes

String GEN_StaticName(DECL_StaticWire w,Arena* out){
  String res = PushString(out,"%.*s_%.*s_%.*s",UN(w.meta->name),UN(w.unitName),UN(w.wire.name));
  return res;
}

// ======================================
// Helpers

void GEN_EmitCombOperations(VEmitter* m,COM_Unit* units,COM_Edge* edges){
  TEMP_REGION(temp,m->arena);
  
  bool combExists = 0;
  for(COM_Unit* unit = units; unit; unit = unit->next){
    if(DECL_IsVersatOperation(unit->decl)){
      combExists = 1;
      break;
    } 
  }

  if(combExists){
    m->CombBlock();
    for(COM_Unit* unit = units; unit; unit = unit->next){
      FUDeclaration* decl  = unit->decl;
      GEN_Data* data = (GEN_Data*) unit->general;
    
      if(DECL_IsVersatOperation(decl)){
        Array<FormatContent> content = ParseFormat(decl->operation,temp);
        Array<String> wireNames = Extract(data->inputInfo,temp,&GEN_OutputData::wireName);

        auto b = StartString(temp);
        for(FormatContent form : content){
          if(form.isText){
            b->PushString(form.text);
          } else {
            int index = form.index;
            b->PushString(wireNames[index]);
          }
        }
        String expr = EndString(temp,b);

        String identifier = data->outputInfo[0].wireName;
        m->Set(identifier,expr);
      }
    }
    m->EndBlock();
  }
}

void GEN_EmitInstanciateUnits(VEmitter* m,COM_Unit* units,COM_Edge* edges){
  TEMP_REGION(temp,m->arena);

  int delaySeen = 0;
  int doneSeen = 0;
  int ioSeen = 0;
  int mmSeen = 0;
  int externalSeen = 0;
  int unitValidSeen = 0;
  int delayInSeen = 0;
  int delayOutSeen = 0;
  
  for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
    GEN_Data* data = (GEN_Data*) ptr->general;
    FUDeclaration* decl = ptr->decl;

    if(decl == BasicDeclaration::input ||
       decl == BasicDeclaration::output ||
       DECL_IsVersatOperation(decl)){
      continue;
    }

    m->StartInstance(decl->name,SF("%.*s_%d",UN(ptr->name),data->index));

    for(DECL_Param p : decl->parameters){
      m->InstanceParam(p.name,p.val);
    }

    for(int i = 0; i < data->outputInfo.size; i++){
      if(data->outputInfo[i].isConnected){
        m->PortConnectIndexed("out%d",i,data->outputInfo[i].wireName);
      } else {
        m->PortConnectIndexed("out%d",i,"");
      }
    }

    for(int i = 0; i < data->inputInfo.size; i++){
      if(data->inputInfo[i].isConnected){
        m->PortConnectIndexed("in%d",i,data->inputInfo[i].wireName);
      } else {
        m->PortConnectIndexed("in%d",i,"0");
      }
    }

    for(Wire w : decl->configs){
      m->PortConnect(w.name,PushString(temp,"%.*s_%.*s",UN(ptr->name),UN(w.name)));
    }
    for(Wire w : decl->states){
      m->PortConnect(w.name,PushString(temp,"%.*s_%.*s",UN(ptr->name),UN(w.name)));
    }
    for(int i = 0; i < decl->numberInputDelays; i++){
      m->PortConnectIndexed("delay_in%d",i,SF("delay_in%d",delayInSeen++));
    }
    for(int i = 0; i < decl->numberOutputDelays; i++){
      m->PortConnectIndexed("delay_out%d",i,SF("delay_out%d",delayOutSeen++));
    }

    for(int i = 0; i < ptr->decl->numberDatabus; i++){
      m->PortConnectIndexed("databus_ready_%d",i,"databus_ready_%d",ioSeen);
      m->PortConnectIndexed("databus_valid_%d",i,"databus_valid_%d",ioSeen);
      m->PortConnectIndexed("databus_addr_%d",i,"databus_addr_%d",ioSeen);
      m->PortConnectIndexed("databus_rdata_%d",i,"databus_rdata_%d",ioSeen);
      m->PortConnectIndexed("databus_wdata_%d",i,"databus_wdata_%d",ioSeen);
      m->PortConnectIndexed("databus_wstrb_%d",i,"databus_wstrb_%d",ioSeen);
      m->PortConnectIndexed("databus_len_%d",i,"databus_len_%d",ioSeen);
      m->PortConnectIndexed("databus_last_%d",i,"databus_last_%d",ioSeen);

      ioSeen += 1;
    }


#if 1
#if 0
    // Static configs
    // TODO: We do not want to iterate the static units. We only care about the wire names.
    for(Pair<StaticId,StaticData*> p : decl->staticUnits){
      StaticId id = p.first;
      for(Wire w : p.second->configs){
        String wireStaticName = PushString(temp,"%.*s_%.*s_%.*s",UN(id.parent->name),UN(id.name),UN(w.name));
          
        m->PortConnect(wireStaticName,wireStaticName);
      }
    }
#endif

    // External memories
    for(int i = 0; i <  decl->externalMemory.size; i++){
      HW_Instance& ext = decl->externalMemory[i];
      if(ext.inter == &HW_Default::HW_DP){
        m->PortConnectIndexed("ext_dp_addr_%d_port_0",i,"ext_dp_addr_%d_port_0",externalSeen);
        m->PortConnectIndexed("ext_dp_out_%d_port_0",i,"ext_dp_out_%d_port_0",externalSeen);
        m->PortConnectIndexed("ext_dp_in_%d_port_0",i,"ext_dp_in_%d_port_0",externalSeen);
        m->PortConnectIndexed("ext_dp_enable_%d_port_0",i,"ext_dp_enable_%d_port_0",externalSeen);
        m->PortConnectIndexed("ext_dp_write_%d_port_0",i,"ext_dp_write_%d_port_0",externalSeen);

        m->PortConnectIndexed("ext_dp_addr_%d_port_1",i,"ext_dp_addr_%d_port_1",externalSeen);
        m->PortConnectIndexed("ext_dp_out_%d_port_1",i,"ext_dp_out_%d_port_1",externalSeen);
        m->PortConnectIndexed("ext_dp_in_%d_port_1",i,"ext_dp_in_%d_port_1",externalSeen);
        m->PortConnectIndexed("ext_dp_enable_%d_port_1",i,"ext_dp_enable_%d_port_1",externalSeen);
        m->PortConnectIndexed("ext_dp_write_%d_port_1",i,"ext_dp_write_%d_port_1",externalSeen);
      } else {
        m->PortConnectIndexed("ext_2p_addr_out_%d",i,"ext_2p_addr_out_%d",externalSeen);
        m->PortConnectIndexed("ext_2p_addr_in_%d",i,"ext_2p_addr_in_%d",externalSeen);
        m->PortConnectIndexed("ext_2p_write_%d",i,"ext_2p_write_%d",externalSeen);
        m->PortConnectIndexed("ext_2p_read_%d",i,"ext_2p_read_%d",externalSeen);
        m->PortConnectIndexed("ext_2p_data_in_%d",i,"ext_2p_data_in_%d",externalSeen);
        m->PortConnectIndexed("ext_2p_data_out_%d",i,"ext_2p_data_out_%d",externalSeen);
      }
      
      externalSeen += 1;
    }

    // Memory mapping
    if(decl->memoryMapped.size > 0){
      int addrSize = 0;
      for(int i = 0; i < decl->memoryMapped.size; i++){
        SYM_Expr sizeExpr = HW_GetAddrSize(decl->memoryMapped[0].inst,0);
        SYM_EvaluateResult res = SYM_ConstantEvaluate(sizeExpr);
        Assert(!res.Error());
        addrSize = MAX(addrSize,res.result);
      }

      m->PortConnect("wstrb","wstrb");
      m->PortConnect("addr",SF("addr[%d-1:0]",addrSize));
      m->PortConnect("rdata",SF("unitRData[%d]",mmSeen));
      m->PortConnect("rvalid",SF("unitRValid[%d]",mmSeen));
      m->PortConnect("wdata","wdata");

      if(DECL_IsComposite(decl)){
        for(int i = 0; i < decl->memoryMapped.size; i++){
          m->PortConnectIndexed("unit_valid_%d",i,SF("unit_valid_%d",unitValidSeen++));
        }
      } else {
        m->PortConnect("valid",SF("unit_valid_%d",unitValidSeen++));
      }
      mmSeen += 1;
    }
#endif

    if(decl->singleInterfaces & DECL_SingleInterface_LOOP){
      m->PortConnect("signal_loop","signal_loop");
    }
    if(decl->singleInterfaces & DECL_SingleInterface_RUNNING){
      m->PortConnect("running","running");
    }
    if(decl->singleInterfaces & DECL_SingleInterface_RUN){
      m->PortConnect("run","run");
    }
    if(decl->singleInterfaces & DECL_SingleInterface_DONE){
      m->PortConnect("done",SF("unitDone[%d]",doneSeen++));
    }
    if(decl->singleInterfaces & DECL_SingleInterface_CLK){
      m->PortConnect("clk","clk");
    }
    if(decl->singleInterfaces & DECL_SingleInterface_RESET){
      m->PortConnect("rst","rst");
    }
      
    m->EndInstance();
  }
}

void GEN_EmitConnectOutputsToOut(VEmitter* v,COM_Unit* units,COM_Edge* edges){
  TEMP_REGION(temp,v->arena);

  // TODO: If we just pass the output unit directly we can the loop and just do things directly.
  for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
    if(ptr->decl == BasicDeclaration::output){
      GEN_Data* data = (GEN_Data*) ptr->general;

      for(int i = 0; i < data->inputInfo.size; i++){
        if(data->inputInfo[i].isConnected){
          v->Assign(PushString(temp,"out%d",i),data->inputInfo[i].wireName);
        }
      }

      break;
    }
  }
}

void GEN_InitializeAuxGraphData(COM_Unit* units,COM_Edge* edges,Arena* outArena){
  int index = 0;
  for(COM_Unit* ptr = units; ptr; index++,ptr = ptr->next){
    GEN_Data* data = PushStruct<GEN_Data>(outArena);
    data->inputInfo = PushArray<GEN_OutputData>(outArena,ptr->numberInputs);
    data->outputInfo = PushArray<GEN_OutputData>(outArena,ptr->decl->outputs.size);
    data->index = index;

    ptr->general = data;
  }

  for(COM_Edge* edge = edges; edge; edge = edge->next){
    GEN_Data* dataOut = (GEN_Data*) edge->out->general;
    GEN_Data* dataIn = (GEN_Data*) edge->in->general;
      
    COM_Unit* out = edge->out;

    dataOut->outputInfo[edge->outPort].isConnected = 1;
      
    String wireName = dataOut->outputInfo[edge->outPort].wireName;
    if(Empty(dataOut->outputInfo[edge->outPort].wireName)){
      bool isReg = 0;
      bool declare = 1;
      if(DECL_IsVersatOperation(out->decl)){
        wireName = PushString(outArena,"comb_%.*s_%d",UN(out->decl->name),dataOut->index);
        isReg = 1;
      } else if(out->decl == BasicDeclaration::input){
        wireName = PushString(outArena,"in%d",out->special);
        declare = 0;
      } else {
        wireName = PushString(outArena,"output_%d_%d",dataOut->index,edge->outPort);
      }

      dataOut->outputInfo[edge->outPort].wireName = wireName;
      dataOut->outputInfo[edge->outPort].isReg = isReg;
      dataOut->outputInfo[edge->outPort].declare = declare;
    }

    dataIn->inputInfo[edge->inPort].isConnected = 1;

    // NOTE: Store the name of the output inside the input unit since it is already know at this point.
    //       Since inputs can only connect once this assert can only trigger due to some problem with the data.
    //       Which should not happen at this point
    Assert(Empty(dataIn->inputInfo[edge->inPort].wireName));
    dataIn->inputInfo[edge->inPort].wireName = wireName;
  }
}


String GEN_OutputCircuitSource(FUDeclaration* module,Arena* out){
  TEMP_REGION(temp,out);
  FREE_ARENA(codeArena);

  // Initialize data ============================================================
  COM_Unit* units = module->baseGraph.units.head;
  COM_Edge* edges = module->baseGraph.edges.head;

  GEN_InitializeAuxGraphData(units,edges,temp);

  VEmitter* m = StartVCode(codeArena);

  m->Timescale("1ns","1ps");

  m->Module(module->name);

  // Interface ==================================================================
  for(DECL_Param p : module->parameters){
    m->ModuleParam(p.name,SYM_0);
  }
  
  if(module->singleInterfaces & DECL_SingleInterface_RUN){ m->Input("run"); }
  if(module->singleInterfaces & DECL_SingleInterface_RUNNING){ m->Input("running"); }
  if(module->singleInterfaces & DECL_SingleInterface_CLK){ m->Input("clk"); }
  if(module->singleInterfaces & DECL_SingleInterface_RESET){ m->Input("rst"); }
  if(module->singleInterfaces & DECL_SingleInterface_DONE){ m->Output("done"); }
  if(module->singleInterfaces & DECL_SingleInterface_LOOP){ m->Input("signal_loop"); }
  
  for(int i = 0; i < module->inputs.size; i++){
    m->InputIndexed("in%d",i,SYM_DataW);
  }
  for(int i = 0; i < module->outputs.size; i++){
    m->OutputIndexed("out%d",i,SYM_DataW);
  }
  for(Wire w : module->configs){
    m->Input(w.name,w.sizeExpr);
  }
  
  for(DECL_StaticWire w : module->statics){
    String repr = GEN_StaticName(w,temp);
    m->Input(repr,w.wire.sizeExpr);
  }

  for(Wire w : module->states){
    m->Output(w.name,w.sizeExpr);
  }

  for(int i = 0; i < module->numberInputDelays; i++){
    m->InputIndexed("delay_in%d",i,SYM_DelayW);
  }
  for(int i = 0; i < module->numberOutputDelays; i++){
    m->InputIndexed("delay_out%d",i,SYM_DelayW);
  }

  // Databus interface
  for(int i = 0; i < module->numberDatabus; i++){
    m->InputIndexed("databus_ready_%d",i);
    m->OutputIndexed("databus_valid_%d",i);
    m->OutputIndexed("databus_addr_%d",i,SYM_AxiAddrW);
    m->InputIndexed("databus_rdata_%d",i,SYM_AxiDataW);
    m->OutputIndexed("databus_wdata_%d",i,SYM_AxiDataW);
    m->OutputIndexed("databus_wstrb_%d",i,SYM_AxiStrobeW);
    m->OutputIndexed("databus_len_%d",i,SYM_LenW);
    m->InputIndexed("databus_last_%d",i);
  }

  for(HW_Instance& inst : module->externalMemory){
    HW_EmitPortDeclaration(m,&inst,true);
  }

  if(module->memoryMapped.size){
    for(int i = 0; i < module->memoryMapped.size; i++){
      m->InputIndexed("unit_valid_%d",i);
    }

    m->Input("addr",SYM_AddrW);
    m->Input("wstrb",SYM_DataStrobeW);
    m->Input("wdata",SYM_DataW);
    m->Output("rvalid");
    m->Output("rdata","DATA_W");
  }

  // Memory mapped units
  if(module->memoryMapped.size >= 1){
    m->Wire("unitRValid",module->memoryMapped.size);
    m->Assign("rvalid","(|unitRValid)");

    m->WireArray("unitRData",module->memoryMapped.size,"DATA_W");
    m->WireAndAssignJoinBlock("unitRDataFinal","|","DATA_W");
    for(int i = 0; i < module->memoryMapped.size; i++){
      m->Expression(SF("unitRData[%d]",i));
    }
    m->EndBlock();
    m->Assign("rdata","unitRDataFinal");
  }

  if(module->numberDones){
    m->Wire("unitDone",module->numberDones);
    m->Assign("done","&unitDone");
  }

  for(COM_Unit* unit = units; unit; unit = unit->next){
    GEN_Data* data = (GEN_Data*) unit->general;

    for(int i = 0; i < data->outputInfo.size; i++){
      if(data->outputInfo[i].isConnected && data->outputInfo[i].declare){
        if(data->outputInfo[i].isReg){
          m->Reg(data->outputInfo[i].wireName,"DATA_W");
        } else {
          m->Wire(data->outputInfo[i].wireName,"DATA_W");
        }
      }
    }
  }

  GEN_EmitCombOperations(m,units,edges);
  GEN_EmitInstanciateUnits(m,units,edges);
  GEN_EmitConnectOutputsToOut(m,units,edges);

  m->EndModule();
  
  VAST* ast = EndVCode(m);
  StringBuilder* b = StartString(temp);
  Repr(ast,b);
  String content = EndString(out,b);

  COM_ClearGeneralPtr(units);

  return content;
}

GEN_TopContent GEN_OutputTopLevel(GEN_TopInfo* info,Arena* out){
  TEMP_REGION(temp,out);

  FUDeclaration* decl = info->decl;

  String versatDefines = {};
  String versatUndefs = {};

  {
    SYM_EvaluateResult configSize = SYM_ConstantEvaluate(info->configTotalBitCount);
    SYM_EvaluateResult stateSize = SYM_ConstantEvaluate(info->stateBitCount);

    auto d = StartString(temp);
    auto u = StartString(temp);

    d->PushString("`define CONFIG_W %d\n",configSize.result);
    u->PushString("`undef  CONFIG_W\n");
    d->PushString("`define STATE_W %d\n",stateSize.result);
    u->PushString("`undef  STATE_W\n");
    d->PushString("`define nIO %d\n",info->numberDatabus);
    u->PushString("`undef  nIO\n");
    d->PushString("`define LEN_W %d\n",20);
    u->PushString("`undef  LEN_W\n");

    if(globalOptions.architectureHasDatabus){
      d->PushString("`define VERSAT_ARCH_HAS_IO 1\n");
      u->PushString("`undef  VERSAT_ARCH_HAS_IO\n");
    }

    if(info->numberDatabus){
      d->PushString("`define VERSAT_IO\n");
      u->PushString("`undef  VERSAT_IO\n");
    }

    if(decl->externalMemory.size){
      d->PushString("`define VERSAT_EXTERNAL_MEMORY\n");
      u->PushString("`undef  VERSAT_EXTERNAL_MEMORY\n");
    }

    if(globalOptions.exportInternalMemories){
      d->PushString("`define VERSAT_EXPORT_EXTERNAL_MEMORY\n");
      u->PushString("`undef  VERSAT_EXPORT_EXTERNAL_MEMORY\n");
    }

    versatDefines = EndString(out,d);
    versatUndefs = EndString(out,u);
  }

  // NOTE: We want to minimize the amount of changes in the top filepath ==========
  String topHardwareWrapper = {};
  {
    TE_Clear();

    TE_SetString("prefix",globalOptions.prefixIObPort);
    
    if(globalOptions.extraIOb){
      TE_SetString("extraIob","iob_csrs_iob_rready_i,");
    } else {
      TE_SetString("extraIob",{});
    }

    if(globalOptions.useSymbolAddress){
      TE_SetString("AXIAddr",R"FOO(
assign axi_awaddr_o = temp_axi_awaddr_o[AXI_ADDR_W-3:2];
assign axi_araddr_o = temp_axi_araddr_o[AXI_ADDR_W-3:2];
)FOO");
    } else {
      TE_SetString("AXIAddr",R"FOO(
assign axi_awaddr_o = temp_axi_awaddr_o;
assign axi_araddr_o = temp_axi_araddr_o;
)FOO");
    }

    if(globalOptions.useSymbolAddress){
      TE_SetString("addr",PushString(temp,".csr_addr({%.*siob_addr_i,2'b00}),",UN(globalOptions.prefixIObPort)));
    } else {
      TE_SetString("addr",PushString(temp,".csr_addr(%.*siob_addr_i),",UN(globalOptions.prefixIObPort)));
    }
    
    topHardwareWrapper = TE_ProcessTemplate(out,META_VersatTemplate);
  }

  String externalMemInst = {};
  {
    VEmitter* m = StartVCode(temp);

    for(int i = 0; i < decl->externalMemory.size; i++){
      HW_Instance ext = decl->externalMemory[i];
      SYM_Expr addrSize = HW_GetAddrSize(ext,0);
      Assert(!IsNil(addrSize));

      if(HW_IsMemoryDP(ext.inter)){
        SYM_Expr data0Size = HW_GetDataSize(ext,0);
        SYM_Expr data1Size = HW_GetDataSize(ext,1);
        Assert(!IsNil(data0Size) && !IsNil(data1Size));

        m->StartInstance(DP_ModuleName,SF("ext_dp_%d",i));
        m->InstanceParam("A_DATA_W",data0Size);
        m->InstanceParam("B_DATA_W",data1Size);
        m->InstanceParam("ADDR_W",addrSize);

        m->PortConnect("dinA_i",SF("VERSAT0_ext_dp_out_%d_port_0_o",i));
        m->PortConnect("addrA_i",SF("VERSAT0_ext_dp_addr_%d_port_0_o",i));
        m->PortConnect("enA_i",SF("VERSAT0_ext_dp_enable_%d_port_0_o",i));
        m->PortConnect("weA_i",SF("VERSAT0_ext_dp_write_%d_port_0_o",i));
        m->PortConnect("doutA_o",SF("VERSAT0_ext_dp_in_%d_port_0_i",i));

        m->PortConnect("dinB_i",SF("VERSAT0_ext_dp_out_%d_port_1_o",i));
        m->PortConnect("addrB_i",SF("VERSAT0_ext_dp_addr_%d_port_1_o",i));
        m->PortConnect("enB_i",SF("VERSAT0_ext_dp_enable_%d_port_1_o",i));
        m->PortConnect("weB_i",SF("VERSAT0_ext_dp_write_%d_port_1_o",i));
        m->PortConnect("doutB_o",SF("VERSAT0_ext_dp_in_%d_port_1_i",i));

        m->PortConnect("clk_i","clk_i");
        m->EndInstance();
      }
      if(HW_IsMemory2P(ext.inter)){
        SYM_Expr dataOutSize = HW_GetDataSize(ext,0,Direction_OUTPUT);
        SYM_Expr dataInSize = HW_GetDataSize(ext,0,Direction_INPUT);
        
        m->StartInstance(TWO_P_ModuleName,SF("ext_2p_%d",i));
        m->InstanceParam("W_DATA_W",dataOutSize);
        m->InstanceParam("R_DATA_W",dataInSize);
        m->InstanceParam("ADDR_W",addrSize);

        m->PortConnect("w_en_i",SF("VERSAT0_ext_2p_write_%d_o",i));
        m->PortConnect("w_addr_i",SF("VERSAT0_ext_2p_addr_out_%d_o",i));
        m->PortConnect("w_data_i",SF("VERSAT0_ext_2p_data_out_%d_o",i));

        m->PortConnect("r_en_i",SF("VERSAT0_ext_2p_read_%d_o",i));
        m->PortConnect("r_addr_i",SF("VERSAT0_ext_2p_addr_in_%d_o",i));
        m->PortConnect("r_data_o",SF("VERSAT0_ext_2p_data_in_%d_i",i));

        m->PortConnect("clk_i","clk_i");
        m->EndInstance();
      }
    }

    VAST* ast = EndVCode(m);
    StringBuilder* b = StartString(temp);
    Repr(ast,b);
    externalMemInst = EndString(out,b);
  }

  String externalMemWires = {};
  {
    VEmitter* m = StartVCode(temp);

    for(int i = 0; i < decl->externalMemory.size; i++){
      HW_Instance ext = decl->externalMemory[i];
      SYM_Expr addrSize = HW_GetAddrSize(ext,0);
      Assert(!IsNil(addrSize));

      if(HW_IsMemoryDP(ext.inter)){
        SYM_Expr data0Size = HW_GetDataSize(ext,0);
        SYM_Expr data1Size = HW_GetDataSize(ext,1);
        SYM_Expr dataSize[2] = {data0Size,data1Size};
        Assert(!IsNil(data0Size) && !IsNil(data1Size));

        for(int k = 0; k < 2; k++){
          m->Wire(SF("ext_dp_addr_%d_port_%d_o",i,k),addrSize);
          m->Wire(SF("ext_dp_out_%d_port_%d_o",i,k),dataSize[k]);
          m->Wire(SF("ext_dp_in_%d_port_%d_i",i,k),dataSize[k]);
          m->Wire(SF("ext_dp_enable_%d_port_%d_o",i,k));
          m->Wire(SF("ext_dp_write_%d_port_%d_o",i,k));

          m->Wire(SF("VERSAT0_ext_dp_addr_%d_port_%d_o",i,k),addrSize);
          m->Wire(SF("VERSAT0_ext_dp_out_%d_port_%d_o",i,k),dataSize[k]);
          m->Wire(SF("VERSAT0_ext_dp_in_%d_port_%d_i",i,k),dataSize[k]);
          m->Wire(SF("VERSAT0_ext_dp_enable_%d_port_%d_o",i,k));
          m->Wire(SF("VERSAT0_ext_dp_write_%d_port_%d_o",i,k));

          m->Assign(PushString(temp,"VERSAT0_ext_dp_addr_%d_port_%d_o",i,k),PushString(temp,"ext_dp_addr_%d_port_%d_o",i,k));
          m->Assign(PushString(temp,"VERSAT0_ext_dp_out_%d_port_%d_o",i,k),PushString(temp,"ext_dp_out_%d_port_%d_o",i,k));
          m->Assign(PushString(temp,"ext_dp_in_%d_port_%d_i",i,k),PushString(temp,"VERSAT0_ext_dp_in_%d_port_%d_i",i,k));
          m->Assign(PushString(temp,"VERSAT0_ext_dp_enable_%d_port_%d_o",i,k),PushString(temp,"ext_dp_enable_%d_port_%d_o",i,k));
          m->Assign(PushString(temp,"VERSAT0_ext_dp_write_%d_port_%d_o",i,k),PushString(temp,"ext_dp_write_%d_port_%d_o",i,k));
        }
      }

      if(HW_IsMemory2P(ext.inter)){
        SYM_Expr dataOutSize = HW_GetDataSize(ext,0,Direction_OUTPUT);
        SYM_Expr dataInSize = HW_GetDataSize(ext,0,Direction_INPUT);
        Assert(!IsNil(dataOutSize) && !IsNil(dataInSize));

        m->Wire(SF("ext_2p_addr_out_%d_o",i),addrSize);
        m->Wire(SF("ext_2p_addr_in_%d_o",i),addrSize);
        m->Wire(SF("ext_2p_data_in_%d_i",i),dataInSize);
        m->Wire(SF("ext_2p_data_out_%d_o",i),dataOutSize);
        m->Wire(SF("ext_2p_write_%d_o",i));
        m->Wire(SF("ext_2p_read_%d_o",i));

        m->Wire(SF("VERSAT0_ext_2p_addr_out_%d_o",i),addrSize);
        m->Wire(SF("VERSAT0_ext_2p_addr_in_%d_o",i),addrSize);
        m->Wire(SF("VERSAT0_ext_2p_data_in_%d_i",i),dataInSize);
        m->Wire(SF("VERSAT0_ext_2p_data_out_%d_o",i),dataOutSize);
        m->Wire(SF("VERSAT0_ext_2p_write_%d_o",i));
        m->Wire(SF("VERSAT0_ext_2p_read_%d_o",i));

        m->Assign(PushString(temp,"VERSAT0_ext_2p_addr_out_%d_o",i),PushString(temp,"ext_2p_addr_out_%d_o",i));
        m->Assign(PushString(temp,"VERSAT0_ext_2p_addr_in_%d_o",i),PushString(temp,"ext_2p_addr_in_%d_o",i));
        m->Assign(PushString(temp,"ext_2p_data_in_%d_i",i),PushString(temp,"VERSAT0_ext_2p_data_in_%d_i",i));
        m->Assign(PushString(temp,"VERSAT0_ext_2p_data_out_%d_o",i),PushString(temp,"ext_2p_data_out_%d_o",i));
        m->Assign(PushString(temp,"VERSAT0_ext_2p_write_%d_o",i),PushString(temp,"ext_2p_write_%d_o",i));
        m->Assign(PushString(temp,"VERSAT0_ext_2p_read_%d_o",i),PushString(temp,"ext_2p_read_%d_o",i));
      }
    }

    VAST* ast = EndVCode(m);
    StringBuilder* b = StartString(temp);
    Repr(ast,b);
    externalMemWires = EndString(out,b);
  }

  String externalMemPorts = {};
  {
    VEmitter* m = StartVCode(temp);

    m->StartPortGroup();
  
    for(int i = 0; i < decl->externalMemory.size; i++){
      HW_Instance ext = decl->externalMemory[i];
      SYM_Expr addrSize = HW_GetAddrSize(ext,0);
      Assert(!IsNil(addrSize));

      if(HW_IsMemoryDP(ext.inter)){
        SYM_Expr data0Size = HW_GetDataSize(ext,0);
        SYM_Expr data1Size = HW_GetDataSize(ext,1);
        SYM_Expr dataSize[2] = {data0Size,data1Size};
        Assert(!IsNil(data0Size) && !IsNil(data1Size));

        for(int k = 0; k < 2; k++){
          m->Output(SF("ext_dp_addr_%d_port_%d_o",i,k),addrSize);
          m->Output(SF("ext_dp_out_%d_port_%d_o",i,k),dataSize[k]);
          m->Input(SF("ext_dp_in_%d_port_%d_i",i,k),dataSize[k]);
          m->Output(SF("ext_dp_enable_%d_port_%d_o",i,k));
          m->Output(SF("ext_dp_write_%d_port_%d_o",i,k));
        }
      }

      if(HW_IsMemory2P(ext.inter)){
        SYM_Expr dataOutSize = HW_GetDataSize(ext,0,Direction_OUTPUT);
        SYM_Expr dataInSize = HW_GetDataSize(ext,0,Direction_INPUT);
        Assert(!IsNil(dataOutSize) && !IsNil(dataInSize));

        m->Output(SF("ext_2p_write_%d_o",i));
        m->Output(SF("ext_2p_addr_out_%d_o",i),addrSize);
        m->Output(SF("ext_2p_data_out_%d_o",i),dataOutSize);

        m->Output(SF("ext_2p_read_%d_o",i));
        m->Output(SF("ext_2p_addr_in_%d_o",i),addrSize);
        m->Input(SF("ext_2p_data_in_%d_i",i),dataInSize);
      }
    }

    m->EndPortGroup();

    VAST* ast = EndVCode(m);
    StringBuilder* b = StartString(temp);
    Repr(ast,b);
    externalMemPorts = EndString(out,b);
  }

  String externalMemPortmap = {};
  {
    VEmitter* m = StartVCode(temp);

    for(int i = 0; i < decl->externalMemory.size; i++){
      HW_Instance ext = decl->externalMemory[i];

      if(HW_IsMemoryDP(ext.inter)){
        for(int k = 0; k < 2; k++){
          m->PortConnect(PushString(temp,"ext_dp_out_%d_port_%d_o",i,k),PushString(temp,"ext_dp_out_%d_port_%d_o",i,k));
          m->PortConnect(PushString(temp,"ext_dp_addr_%d_port_%d_o",i,k),PushString(temp,"ext_dp_addr_%d_port_%d_o",i,k));
          m->PortConnect(PushString(temp,"ext_dp_enable_%d_port_%d_o",i,k),PushString(temp,"ext_dp_enable_%d_port_%d_o",i,k));
          m->PortConnect(PushString(temp,"ext_dp_write_%d_port_%d_o",i,k),PushString(temp,"ext_dp_write_%d_port_%d_o",i,k));
          m->PortConnect(PushString(temp,"ext_dp_in_%d_port_%d_i",i,k),PushString(temp,"ext_dp_in_%d_port_%d_i",i,k));
        }
      }

      if(HW_IsMemory2P(ext.inter)){
        m->PortConnect(PushString(temp,"ext_2p_write_%d_o",i),PushString(temp,"ext_2p_write_%d_o",i));
        m->PortConnect(PushString(temp,"ext_2p_addr_out_%d_o",i),PushString(temp,"ext_2p_addr_out_%d_o",i));
        m->PortConnect(PushString(temp,"ext_2p_data_out_%d_o",i),PushString(temp,"ext_2p_data_out_%d_o",i));

        m->PortConnect(PushString(temp,"ext_2p_read_%d_o",i),PushString(temp,"ext_2p_read_%d_o",i));
        m->PortConnect(PushString(temp,"ext_2p_addr_in_%d_o",i),PushString(temp,"ext_2p_addr_in_%d_o",i));
        m->PortConnect(PushString(temp,"ext_2p_data_in_%d_i",i),PushString(temp,"ext_2p_data_in_%d_i",i));
      }
    }

    VAST* ast = EndVCode(m);
    StringBuilder* b = StartString(temp);
    Repr(ast,b);
    externalMemPortmap = EndString(out,b);
  }
  
    // TODO: Need to add some form of error checking and handling inside the script for the case where verilator root is not found
  String getVerilatorRoot = META_GetVerilatorRoot;
  String extractVerilatedSignal = META_ExtractVerilatedSignals;

  GEN_TopContent res = {};
  res.verilogDefines = versatDefines;
  res.verilogUndefs = versatUndefs;
  res.externalMemInst = externalMemInst;
  res.externalMemWires = externalMemWires;
  res.externalMemPorts = externalMemPorts;
  res.externalMemPortmap = externalMemPortmap;
  res.hardwareInstance = GEN_OutputHardwareInstance(info,out);
  res.hardwareConfigs = GEN_OutputHardwareConfigurations(info,out);
  res.headerFile = GEN_OutputHeader(info,out);
  res.verilatorSoftwareWrapper = GEN_OutputVerilatorSoftwareWrapper(info,out);
  res.verilatorHardwareWrapper = GEN_OutputVerilatorHardwareWrapper(info,out);
  res.softwareFirmware = GEN_OutputFirmware(info,out);
  res.topHardwareWrapper = topHardwareWrapper;
  res.getVerilatorRoot = getVerilatorRoot;
  res.extractVerilatedSignal = extractVerilatedSignal;

  return res;
}

String GEN_OutputHardwareInstance(GEN_TopInfo* info,Arena* out){
  TEMP_REGION(temp,out);

  FUDeclaration* decl = info->decl;
  COM_Unit* units = decl->baseGraph.units.head;
  COM_Edge* edges = decl->baseGraph.edges.head;

  int stateCount = info->stateCount;
  int databusCount = info->numberDatabus;
  int donesCount = decl->numberDones;
  int memoryMappedCount = decl->memoryMapped.size;
  int memoryConfigDecisionBit = info->memoryConfigDecisionBit;

  SYM_Expr configBitCount = info->configTotalBitCount;
  SYM_Expr stateBitCount = info->stateBitCount;

  auto AddrIf = [info](VEmitter* m,VersatRegister r){
    int index = GEN_GetRegisterIndex(info,r);
    m->If(SF("csr_addr >= %d && csr_addr < %d",index * 4,(index + 1) * 4));
  };

  // Inputs and Outputs =========================================================
  {
    VEmitter* m = StartVCode(temp);
    m->StartPortGroup();

    for(int i = 0; i < decl->externalMemory.size; i++){
      HW_Instance ext = decl->externalMemory[i];
      SYM_Expr addrSize = HW_GetAddrSize(ext,0);
      Assert(!IsNil(addrSize));

      if(HW_IsMemoryDP(ext.inter)){
        SYM_Expr data0Size = HW_GetDataSize(ext,0);
        SYM_Expr data1Size = HW_GetDataSize(ext,1);
        SYM_Expr dataSize[2] = {data0Size,data1Size};
        Assert(!IsNil(data0Size) && !IsNil(data1Size));

        for(int k = 0; k < 2; k++){
          m->Output(SF("ext_dp_addr_%d_port_%d_o",i,k),addrSize);
          m->Output(SF("ext_dp_out_%d_port_%d_o",i,k),dataSize[k]);
          m->Input(SF("ext_dp_in_%d_port_%d_i",i,k),dataSize[k]);
          m->Output(SF("ext_dp_enable_%d_port_%d_o",i,k));
          m->Output(SF("ext_dp_write_%d_port_%d_o",i,k));
        }
      }

      if(HW_IsMemory2P(ext.inter)){
        SYM_Expr dataOutSize = HW_GetDataSize(ext,0,Direction_OUTPUT);
        SYM_Expr dataInSize = HW_GetDataSize(ext,0,Direction_INPUT);
        Assert(!IsNil(dataOutSize) && !IsNil(dataInSize));

        m->Output(SF("ext_2p_write_%d_o",i));
        m->Output(SF("ext_2p_addr_out_%d_o",i),addrSize);
        m->Output(SF("ext_2p_data_out_%d_o",i),dataOutSize);

        m->Output(SF("ext_2p_read_%d_o",i));
        m->Output(SF("ext_2p_addr_in_%d_o",i),addrSize);
        m->Input(SF("ext_2p_data_in_%d_i",i),dataInSize);
      }
    }

    m->EndPortGroup();
    auto b = StartString(temp);
    Repr(EndVCode(m),b);
    String content = EndString(temp,b);
    TE_SetString("externalMemPorts",content);
  }

  TE_SetString("inAndOuts",{});

  {
    VEmitter* m = StartVCode(temp);

    Array<HW_Instance> databus = PushArray<HW_Instance>(temp,info->numberDatabus);
    for(int i = 0; i < databus.size; i++){
      databus[i] = HW_Instantiate(&HW_Default::HW_DATABUS,{},temp);
      databus[i].inter->schema = HW_RemoveFromSchema(databus[i].inter->schema,HW_SchemaType_INTERFACE,temp);
      databus[i].index = i;
    }

    HW_EmitUnpacking(m,databus,"m","",'_',true);
    VAST* ast = EndVCode(m);

    auto b = StartString(temp);
    Repr(ast,b);
    String content = EndString(temp,b);
    TE_SetString("emitIO",content);
  }
  
  {
    VEmitter* m = StartVCode(temp);
    GEN_EmitConnectOutputsToOut(m,units,edges);
    auto b = StartString(temp);
    Repr(EndVCode(m),b);
    String content = EndString(temp,b);
      
    TE_SetString("assignOuts",content);
  }

  {
    VEmitter* m = StartVCode(temp);

    if(decl->singleInterfaces & DECL_SingleInterface_DONE){
      m->Wire("unitDone",SYM_1);
    } else {
      m->WireAndAssignJoinBlock("unitDone","",1);
      m->Expression("1'b1");
      m->EndBlock();
    }

    if(databusCount){
      m->Wire("unitRdataFinal","DATA_W");
    }
    auto b = StartString(temp);
    Repr(EndVCode(m),b);
    String content = EndString(temp,b);

    TE_SetString("wireDecls",content);
  }
  
  if(globalOptions.insertProfilingRegisters){
    VEmitter* m = StartVCode(temp);
    
    SYM_Expr doubleDataW = SYM_DataW * SYM_2;

    m->Reg("profile_runCount",doubleDataW);
    m->Reg("profile_cycles",doubleDataW);
    m->Reg("profile_runningCycles",doubleDataW);
    m->Reg("profile_databusValid",doubleDataW);
    m->Reg("profile_databusValidAndReady",doubleDataW);
    m->Reg("profile_configurationsSet",doubleDataW);
    m->Reg("profile_configurationsSetWhileRunning",doubleDataW);

    if(!SYM_IsZeroValue(configBitCount)){
      m->Wire("configSet");
      m->Assign("configSet",SF("csr_valid && we && !memoryMappedAddr && data_address >= %d",(info->configStart * 4)));
    }

    m->AlwaysBlock("clk","rst_int");
    m->If("rst_int");
    m->Set("profile_runCount","0");
    m->Set("profile_cycles","0");
    m->Set("profile_runningCycles","0");
    m->Set("profile_databusValid","0");
    m->Set("profile_databusValidAndReady","0");
    m->Set("profile_configurationsSet","0");
    m->Set("profile_configurationsSetWhileRunning","0");

    m->Else();

    m->Increment("profile_cycles");

    m->If("run");
    m->Increment("profile_runCount");
    m->EndIf();

    m->If("running");
    m->Increment("profile_runningCycles");
    m->EndIf();
    
    if(databusCount){
      m->If("|m_databus_valid");
      m->Increment("profile_databusValid");
      m->EndIf();

      m->If("(|m_databus_valid) && (|m_databus_ready)");
      m->Increment("profile_databusValidAndReady");
      m->EndIf();
    }

    if(!SYM_IsZeroValue(configBitCount)){
      m->If("configSet");
      m->Increment("profile_configurationsSet");
      m->EndIf();

      m->If("configSet && running");
      m->Increment("profile_configurationsSetWhileRunning");
      m->EndIf();
    }

    m->If(SF("csr_valid && we && csr_addr >= %d && csr_addr < %d && csr_wstrb[0] == 1'b1 && csr_wdata[0] == 1'b1",GEN_GetRegisterIndex(info,VersatRegister_ProfileControl),GEN_GetRegisterIndex(info,VersatRegister_ProfileControl)+4));
      m->Set("profile_runCount","0");
      m->Set("profile_cycles","0");
      m->Set("profile_runningCycles","0");
      m->Set("profile_databusValid","0");
      m->Set("profile_databusValidAndReady","0");
      m->Set("profile_configurationsSet","0");
      m->Set("profile_configurationsSetWhileRunning","0");
    m->EndIf();

    m->EndIf();
    m->EndBlock();

    String content = EndVCodeAndPrint(m,temp);
    TE_SetString("profilingStuff",content);
  } else {
    TE_SetString("profilingStuff",{});
  }

  if(globalOptions.insertCaptureDatabusRegisters){
    VEmitter* m = StartVCode(temp);

    m->Reg("captureDatabus_IndexAndReadWrite",SYM_DataW);
    m->Reg("captureDatabus_Count",SYM_DataW);
    m->Reg("captureDatabus_SavedAddr",SYM_DataW);
    m->Reg("captureDatabus_SavedValue",SYM_DataW);
    m->Reg("captureDatabus_didCapture",SYM_1);

    m->Reg("captureDatabus_Seen",SYM_DataW);

    m->Wire("captureDatabus_ReadNotWrite",SYM_1);
    m->Wire("captureDatabus_Index",SYM_DataW);
    m->Assign("captureDatabus_ReadNotWrite","captureDatabus_IndexAndReadWrite[0]");
    m->Assign("captureDatabus_Index","{1'b0,captureDatabus_IndexAndReadWrite[1+:31]}");

    String format = R"FOO(
always @(posedge clk,posedge rst) begin
  if(capturedDatabus_valid && capturedDatabus_ready) begin
    capturedDatabus_Seen <= capturedDatabus_Seen + 1;
 
    if(capturedDatabus_Seen == capturedDatabus_Count) begin
      capturedDatabus_SavedAddr <= capturedDatabus_addr;
      captureDatabus_didCapture <= 1'b1;
      if(capturedDatabus_ReadNotWrite) begin
        capturedDatabus_SavedValue <= capturedDatabus_rdata;
      end else begin
        capturedDatabus_SavedValue <= capturedDatabus_wdata;
      end
    end
  end
end
)FOO";
    
    String content = EndVCodeAndPrint(m,temp);
    TE_SetString("captureDatabus",content);
    TE_SetString("captureDatabusLogic",format);
  } else {
    TE_SetString("captureDatabus",{});
    TE_SetString("captureDatabusLogic",{});
  }

  // Control write portion
  {
    VEmitter* m = StartVCode(temp);
    if(globalOptions.debug){
      m->Reg("debugStage",SYM_32);
    }

    m->AlwaysBlock("clk","rst_int");
    m->If("rst_int");
    m->Set("startRunPulse","0");
    m->Set("soft_reset","0");
    m->Set("signal_loop","0");
    if(globalOptions.useDMA){
      m->Set("dma_length","0");
      m->Set("dma_internal_address_start","0");
      m->Set("dma_external_addr_start","0");
    }
    if(globalOptions.insertCaptureDatabusRegisters){
      m->Set("captureDatabus_IndexAndReadWrite","0");
      m->Set("captureDatabus_Count","0");
    }

    if(globalOptions.debug){
      m->Set("debugStage","0");
    }

    m->Else();
    
    m->Set("startRunPulse","0");
    
    m->Set("soft_reset","0");
    m->Set("signal_loop","0");
    m->If("csr_valid && we");

    AddrIf(m,VersatRegister_Control);
      m->If("csr_wstrb[0] == 1'b1");
        m->Set("startRunPulse","1");
      m->EndIf();

      m->If("csr_wstrb[3] == 1'b1");
        m->Set("soft_reset","csr_wdata[31]");
        m->Set("signal_loop","csr_wdata[30]");
      m->EndIf();
    m->EndIf();

    if(globalOptions.debug){
      AddrIf(m,VersatRegister_DebugStage);
        m->Set("debugStage","csr_wdata");
      m->EndIf();
    }

    auto EmitStrobe = [](VEmitter* m,String strobeWire,const char* leftReg,const char* rightReg,int regSize){
      for(int i = 0; i < regSize; i += 8){
        int left = 8;
        if(i + left > regSize){
          left = regSize % 8;
        }
        
        m->If(SF("%.*s[%d]",UN(strobeWire),i/8));
        m->Set(leftReg,SF("%s[%d+:%d]",rightReg,i,left));
        m->EndIf();
      }
    };

    if(globalOptions.useDMA){
      AddrIf(m,VersatRegister_DmaInternalAddress);
      EmitStrobe(m,"csr_wstrb","dma_internal_address_start","csr_wdata",32);
      m->EndIf();
      AddrIf(m,VersatRegister_DmaExternalAddress);
      EmitStrobe(m,"csr_wstrb","dma_external_addr_start","csr_wdata",32);
      m->EndIf();
      AddrIf(m,VersatRegister_DmaTransferLength);
      EmitStrobe(m,"csr_wstrb","dma_length","csr_wdata",24);
      m->EndIf();
    }

    m->EndIf();

    m->EndIf();
    m->EndBlock();

    if(globalOptions.useDMA){
      m->Assign("dma_start",SF("csr_valid && we && csr_addr >= %d && csr_addr < %d && csr_wstrb[0] && csr_wdata[0] == 1'b1",GEN_GetRegisterIndex(info,VersatRegister_DmaControl),GEN_GetRegisterIndex(info,VersatRegister_DmaControl)+4));
    }

    if(globalOptions.insertCaptureDatabusRegisters){
      AddrIf(m,VersatRegister_CaptureDatabusIndexAndReadWrite);
      EmitStrobe(m,"csr_wstrb","captureDatabus_IndexAndReadWrite","csr_wdata",32);
      m->EndIf();
      AddrIf(m,VersatRegister_CaptureDatabusCount);
      EmitStrobe(m,"csr_wstrb","captureDatabus_Count","csr_wdata",32);
      m->EndIf();
    }

    String content3 = EndVCodeAndPrint(m,temp);
    TE_SetString("controlWriteInterface",content3);
  }
      
  // Control read portion
  {      
    VEmitter* m = StartVCode(temp);
    m->AlwaysBlock("clk","rst");
      m->If("rst");
        m->Set("versat_rdata","0");
        m->Set("versat_rvalid","0");
      m->Else();
        m->Set("versat_rvalid","0");
        m->If("csr_valid");
          m->If("!memoryMappedAddr");
            m->Set("versat_rdata","stateRead");
            m->If("csr_wstrb == 0");
              m->Set("versat_rvalid","1");
            m->EndIf();
          m->EndIf();
        AddrIf(m,VersatRegister_Control);
          m->Set("versat_rdata","{31'h0,done}");
        m->EndIf();

        if(globalOptions.useDMA){
          AddrIf(m,VersatRegister_DmaControl);
            m->Set("versat_rdata","{31'h0,!dma_running}");
          m->EndIf();
        }
        if(globalOptions.insertDebugRegisters){

          AddrIf(m,VersatRegister_Debug);
          if(databusCount){
            m->Set("versat_rdata","{31'h0,|m_databus_valid}");
          } else {
            m->Set("versat_rdata","32'h0");
          }
          m->EndIf();
        }
        if(globalOptions.insertProfilingRegisters){
          AddrIf(m,VersatRegister_ProfileRunCount);
          m->Set("versat_rdata","profile_runCount[0+:DATA_W]");
          m->EndIf();
          AddrIf(m,VersatRegister_ProfileRunCount2);
          m->Set("versat_rdata","profile_runCount[DATA_W+:DATA_W]");
          m->EndIf();

          AddrIf(m,VersatRegister_ProfileCyclesSinceLastReset);
          m->Set("versat_rdata","profile_cycles[0+:DATA_W]");
          m->EndIf();
          AddrIf(m,VersatRegister_ProfileCyclesSinceLastReset2);
          m->Set("versat_rdata","profile_cycles[DATA_W+:DATA_W]");
          m->EndIf();

          AddrIf(m,VersatRegister_ProfileRunningCycles);
          m->Set("versat_rdata","profile_runningCycles[0+:DATA_W]");
          m->EndIf();
          AddrIf(m,VersatRegister_ProfileRunningCycles2);
          m->Set("versat_rdata","profile_runningCycles[DATA_W+:DATA_W]");
          m->EndIf();

          AddrIf(m,VersatRegister_ProfileDatabusValid);
          m->Set("versat_rdata","profile_databusValid[0+:DATA_W]");
          m->EndIf();
          AddrIf(m,VersatRegister_ProfileDatabusValid2);
          m->Set("versat_rdata","profile_databusValid[DATA_W+:DATA_W]");
          m->EndIf();

          AddrIf(m,VersatRegister_ProfileDatabusValidAndReady);
          m->Set("versat_rdata","profile_databusValidAndReady[0+:DATA_W]");
          m->EndIf();
          AddrIf(m,VersatRegister_ProfileDatabusValidAndReady2);
          m->Set("versat_rdata","profile_databusValidAndReady[DATA_W+:DATA_W]");
          m->EndIf();

          AddrIf(m,VersatRegister_ProfileConfigurationsSet);
          m->Set("versat_rdata","profile_configurationsSet[0+:DATA_W]");
          m->EndIf();
          AddrIf(m,VersatRegister_ProfileConfigurationsSet2);
          m->Set("versat_rdata","profile_configurationsSet[DATA_W+:DATA_W]");
          m->EndIf();

          AddrIf(m,VersatRegister_ProfileConfigurationsSetWhileRunning);
          m->Set("versat_rdata","profile_configurationsSetWhileRunning[0+:DATA_W]");
          m->EndIf();
          AddrIf(m,VersatRegister_ProfileConfigurationsSetWhileRunning2);
          m->Set("versat_rdata","profile_configurationsSetWhileRunning[DATA_W+:DATA_W]");
          m->EndIf();
        }

        if(globalOptions.insertCaptureDatabusRegisters){
          AddrIf(m,VersatRegister_CaptureDatabusGetAddr);
          m->Set("versat_rdata","captureDatabus_SavedAddr");
          m->EndIf();
          AddrIf(m,VersatRegister_CaptureDatabusGetValue);
          m->Set("versat_rdata","captureDatabus_SavedValue");
          m->EndIf();
          AddrIf(m,VersatRegister_CaptureDatabusGetDidCapture);
          m->Set("versat_rdata","{31'h0,CaptureDatabusGetDidCapture}");
          m->EndIf();
        }

        m->EndIf();

      m->EndIf();

    m->EndBlock();
    
    String content = EndVCodeAndPrint(m,temp);
    TE_SetString("controlReadInterface",content);
  }
  
  {
    if(globalOptions.useDMA){
      String content = R"FOO(
reg [LEN_W-1:0] dma_length;
reg [ADDR_W-1:0] dma_internal_address_start;
reg [AXI_ADDR_W-1:0] dma_external_addr_start;

wire dma_ready;
wire [ADDR_W-1:0] dma_addr_in;
wire [AXI_DATA_W-1:0] dma_rdata;

wire dma_start;

SimpleDMA #(.ADDR_W(ADDR_W),.DATA_W(DATA_W),.AXI_ADDR_W(AXI_ADDR_W)) dma (
  .m_databus_ready(databus_ready[`nIO - 1]),
  .m_databus_valid(databus_valid[`nIO - 1]),
  .m_databus_addr(databus_addr[`nIO - 1]),
  .m_databus_rdata(databus_rdata[`nIO - 1]),
  .m_databus_wdata(databus_wdata[`nIO - 1]),
  .m_databus_wstrb(databus_wstrb[`nIO - 1]),
  .m_databus_len(databus_len[`nIO - 1]),
  .m_databus_last(databus_last[`nIO - 1]),

  .addr_internal(dma_internal_address_start),
  .addr_read(dma_external_addr_start),
  .length(dma_length),

  .run(dma_start),
  .running(dma_running),

  .valid(dma_ready),
  .address(dma_addr_in),
  .data(dma_rdata),

  .clk(clk),
  .rst(rst_int)
);

JoinTwoSimple #(.DATA_W(DATA_W),.ADDR_W(ADDR_W)) joiner (
  .m0_valid(dma_ready),
  .m0_data(dma_rdata),
  .m0_addr(dma_addr_in),
  .m0_wstrb(~0),

  .m1_valid(csr_valid),
  .m1_data(csr_wdata),
  .m1_addr(csr_addr),
  .m1_wstrb(csr_wstrb),

  .s_valid(data_valid),
  .s_data(data_data),
  .s_addr(data_address),
  .s_wstrb(data_wstrb)
);
                           )FOO";
      
      TE_SetString("dmaInstantiation",content);
    } else {
      String content = R"FOO(
assign data_valid = csr_valid;
assign data_address = csr_addr;
assign data_data = csr_wdata;
assign data_wstrb = csr_wstrb;
                           )FOO";
        
      TE_SetString("dmaInstantiation",content);
    }
  }

  {
    if(memoryMappedCount == 0){
      TE_SetString("memoryConfigDecisionExpr","1'b0");
    } else {
      String content = PushString(temp,"data_address[%d]",memoryConfigDecisionBit);
      TE_SetString("memoryConfigDecisionExpr",content);
    }
  }

  if(decl->memoryMapped.size > 0){
    VEmitter* m = StartVCode(temp);

    m->Reg("unit_valids",decl->memoryMapped.size);

    m->CombBlock();
    
    m->Set("unit_valids","0");

    if(memoryMappedCount == 1){
      m->If("data_valid & memoryMappedAddr");
      m->Set("unit_valids[0]","1'b1");
      m->EndIf();
    } else {
#if 0
      for(auto iter = StartIteration(&info); iter.IsValid(); iter = iter.Step()){
        InstanceInfo* unit = iter.CurrentUnit();

        if(unit->isComposite || Empty(unit->globalMemDecisionMask)){
          continue;
        }

        int maxMemoryBit = val.memoryConfigDecisionBit - 1;

        String mask = unit->globalMemDecisionMask;
        m->If(SF("data_valid & memoryMappedAddr & (csr_addr[(%d-1) -: %d] == %d'b%.*s)",maxMemoryBit,mask.size,mask.size,UN(mask)));
        m->Set(SF("unit_valids[%d]",unit->memGlobalIndex),"1'b1");
        m->EndIf();
      }
#endif
    }
    m->EndBlock();

    auto b = StartString(temp);
    Repr(EndVCode(m),b);
    String content = EndString(temp,b);
    TE_SetString("unitsMappedDecl",content);
  } else {
    TE_SetString("unitsMappedDecl",{});
  }

  {
    VEmitter* m = StartVCode(temp);
    if(memoryMappedCount >= 1){
      m->Wire("unitRValid",memoryMappedCount);
      m->Assign("csr_rvalid","versat_rvalid | (|unitRValid)");
      m->Wire("unitRData","DATA_W");
      m->Assign("csr_rdata","versat_rvalid ? versat_rdata : unitRData");
    } else {
      m->Assign("csr_rdata","versat_rdata");
      m->Assign("csr_rvalid","versat_rvalid");
    }

    auto b = StartString(temp);
    Repr(EndVCode(m),b);
    String content = EndString(temp,b);
      
    TE_SetString("unitsRData",content);
  }

  {
    VEmitter* m = StartVCode(temp);
      
    if(!SYM_IsZeroValue(info->configTotalBitCount)){
      m->Wire("configdata",info->configTotalBitCount);

      for(DECL_StaticWire w : decl->statics){
        String name = GEN_StaticName(w,temp);
        m->Wire(name,w.wire.sizeExpr);
      }

      m->StartInstance("versat_configurations","configs");

      // TODO: This should be all the global params that Versat cares about. We need to standardize this and properly define all the global parameters
      m->InstanceParam("ADDR_W","ADDR_W");
      m->InstanceParam("DATA_W","DATA_W");
      m->InstanceParam("AXI_DATA_W","AXI_DATA_W");
      m->InstanceParam("AXI_ADDR_W","AXI_ADDR_W");
      m->InstanceParam("DELAY_W","DELAY_W");
      m->InstanceParam("LEN_W","LEN_W");

      m->PortConnect("config_data_o","configdata");
      m->PortConnect("memoryMappedAddr","memoryMappedAddr");
      m->PortConnect("data_write","csr_valid && we");

      m->PortConnect("address","data_address");
      m->PortConnect("data_wstrb","data_wstrb");
      m->PortConnect("data_data","data_data");

      m->PortConnect("change_config_pulse","pre_run_pulse");

      for(DECL_StaticWire w : decl->statics){
        String name = GEN_StaticName(w,temp);
        m->PortConnect(name,name);
      }

      m->PortConnect("clk_i","clk");
      m->PortConnect("rst_i","rst_int");
        
      m->EndInstance();
    }

    auto b = StartString(temp);
    Repr(EndVCode(m),b);
    String content = EndString(temp,b);
      
    TE_SetString("configurationDecl",content);
  }

  {
    VEmitter* m = StartVCode(temp);

    if(info->stateCount){
      m->Wire("statedata",info->stateBitCount);
    }
      
    m->CombBlock();

    m->Set("stateRead","32'h0");
    m->If("csr_valid & !we & !memoryMappedAddr");

    SYM_Expr stateBitsSeen = SYM_0;
    int addr = info->registers.size;
    for(int i = 0; i < decl->states.size; i++){
      Wire wire = decl->states[i];
      m->If(SF("csr_addr == %d",addr * 4));

      String repr = SYM_Repr(stateBitsSeen,temp);
      String size = SYM_Repr(wire.sizeExpr,temp);

      m->Set("stateRead",SF("statedata[(%.*s)+:%.*s]",UN(repr),UN(size)));
      stateBitsSeen += wire.sizeExpr;
      addr += 1;
      m->EndIf();
    }
      
    m->EndIf();
      
    m->EndBlock();
      
    auto b = StartString(temp);
    Repr(EndVCode(m),b);
    String content = EndString(temp,b);
      
    TE_SetString("stateDecl",content);
  }

  TE_SetNumber("databusDataSize",globalOptions.databusDataSize);
  
  {
    VEmitter* m = StartVCode(temp);
    m->StartInstance(decl->name,decl->name);

    for(DECL_Param p : decl->parameters){
      m->InstanceParam(p.name,p.val);
    }

    // Configs
    for(GEN_Wire w : info->allConfigs){
      if(w.type & GEN_WireType_IS_VERSAT){
        continue;
      }
      String repr = SYM_Repr(w.startBit,temp);
      String size = SYM_Repr(w.size,temp);
      
      m->PortConnect(w.name,PushString(temp,"configdata[(%.*s)+:%.*s]",UN(repr),UN(size)));
    }

    // State
    for(GEN_Wire w : info->allStates){
      if(w.type & GEN_WireType_IS_VERSAT){
        continue;
      }
      String repr = SYM_Repr(w.startBit,temp);
      String size = SYM_Repr(w.size,temp);

      m->PortConnect(w.name,PushString(temp,"statedata[(%.*s)+:%.*s]",UN(repr),UN(size)));
    }

    // External memories
    for(int i = 0; i <  decl->externalMemory.size; i++){
      HW_Instance& ext = decl->externalMemory[i];
      int externalSeen = i;
      if(ext.inter == &HW_Default::HW_DP){
        m->PortConnectIndexed("ext_dp_addr_%d_port_0",i,"ext_dp_addr_%d_port_0_o",externalSeen);
        m->PortConnectIndexed("ext_dp_out_%d_port_0",i,"ext_dp_out_%d_port_0_o",externalSeen);
        m->PortConnectIndexed("ext_dp_in_%d_port_0",i,"ext_dp_in_%d_port_0_i",externalSeen);
        m->PortConnectIndexed("ext_dp_enable_%d_port_0",i,"ext_dp_enable_%d_port_0_o",externalSeen);
        m->PortConnectIndexed("ext_dp_write_%d_port_0",i,"ext_dp_write_%d_port_0_o",externalSeen);

        m->PortConnectIndexed("ext_dp_addr_%d_port_1",i,"ext_dp_addr_%d_port_1_o",externalSeen);
        m->PortConnectIndexed("ext_dp_out_%d_port_1",i,"ext_dp_out_%d_port_1_o",externalSeen);
        m->PortConnectIndexed("ext_dp_in_%d_port_1",i,"ext_dp_in_%d_port_1_i",externalSeen);
        m->PortConnectIndexed("ext_dp_enable_%d_port_1",i,"ext_dp_enable_%d_port_1_o",externalSeen);
        m->PortConnectIndexed("ext_dp_write_%d_port_1",i,"ext_dp_write_%d_port_1_o",externalSeen);
      } else {
        m->PortConnectIndexed("ext_2p_addr_out_%d",i,"ext_2p_addr_out_%d_o",externalSeen);
        m->PortConnectIndexed("ext_2p_addr_in_%d",i,"ext_2p_addr_in_%d_o",externalSeen);
        m->PortConnectIndexed("ext_2p_write_%d",i,"ext_2p_write_%d_o",externalSeen);
        m->PortConnectIndexed("ext_2p_read_%d",i,"ext_2p_read_%d_o",externalSeen);
        m->PortConnectIndexed("ext_2p_data_in_%d",i,"ext_2p_data_in_%d_i",externalSeen);
        m->PortConnectIndexed("ext_2p_data_out_%d",i,"ext_2p_data_out_%d_o",externalSeen);
      }
    }

    if(decl->memoryMapped.size > 0){
      m->PortConnect("wstrb","data_wstrb");
      m->PortConnect("addr",SF("csr_addr[ADDR_W-1:0]"));
      m->PortConnect("rdata",SF("unitRData"));
      m->PortConnect("rvalid",SF("unitRValid"));
      m->PortConnect("wdata","data_data");
        
      if(DECL_IsComposite(decl)){
        for(int i = 0; i < decl->memoryMapped.size; i++){
          m->PortConnectIndexed("unit_valid_%d",i,SF("unit_valids[%d]",i));
        }
      } else {
        m->PortConnect("valid",SF("unit_valids[%d]",0));
      }
    }

    // Databus
    int ioSeen = 0;
    for(int i = 0; i < decl->numberDatabus; i++){
      m->PortConnectIndexed("databus_ready_%d",i,"databus_ready[%d]",ioSeen);
      m->PortConnectIndexed("databus_valid_%d",i,"databus_valid[%d]",ioSeen);
      m->PortConnectIndexed("databus_addr_%d",i,"databus_addr[%d]",ioSeen);
      m->PortConnectIndexed("databus_rdata_%d",i,"databus_rdata[%d]",ioSeen);
      m->PortConnectIndexed("databus_wdata_%d",i,"databus_wdata[%d]",ioSeen);
      m->PortConnectIndexed("databus_wstrb_%d",i,"databus_wstrb[%d]",ioSeen);
      m->PortConnectIndexed("databus_len_%d",i,"databus_len[%d]",ioSeen);
      m->PortConnectIndexed("databus_last_%d",i,"databus_last[%d]",ioSeen);

      ioSeen += 1;
    }

    if(decl->singleInterfaces & DECL_SingleInterface_LOOP){
      m->PortConnect("signal_loop","signal_loop");
    }
    if(decl->singleInterfaces & DECL_SingleInterface_RUNNING){
      m->PortConnect("running","running");
    }
    if(decl->singleInterfaces & DECL_SingleInterface_RUN){
      m->PortConnect("run","run");
    }
    if(decl->singleInterfaces & DECL_SingleInterface_DONE){
      m->PortConnect("done",SF("unitDone"));
    }
    if(decl->singleInterfaces & DECL_SingleInterface_CLK){
      m->PortConnect("clk","clk");
    }
    if(decl->singleInterfaces & DECL_SingleInterface_RESET){
      m->PortConnect("rst","rst");
    }
      
    m->EndInstance();
    
    auto b = StartString(temp);
    Repr(EndVCode(m),b);
    String content = EndString(temp,b);
    TE_SetString("topUnit",content);
  }
    
  String res = TE_ProcessTemplate(out,META_TopInstanceTemplate,true);

  COM_ClearGeneralPtr(units);

  return res;
}

String GEN_OutputVerilatorMakefile(GEN_TopInfo* info,Array<String> unitsGenerated,Arena* out){
  FUDeclaration* decl = info->decl;

  TEMP_REGION(temp,nullptr);
  
  Array<String> allFilenames = PushArray<String>(temp,globalOptions.verilogFiles.size);
  for(int i = 0; i <  globalOptions.verilogFiles.size; i++){
    String filepath  =  globalOptions.verilogFiles[i];
    fs::path path(SF("%.*s",UN(filepath)));
    allFilenames[i] = PushString(temp,"%s",path.filename().c_str());
  }
  
  String generatedUnitsLocation = GetRelativePathFromSourceToTarget(globalOptions.softwareOutputFilepath,globalOptions.hardwareOutputFilepath,temp);
  
  TE_SetString("hardwareFolder",generatedUnitsLocation);
  
  {
    auto s = StartString(temp);
    for(String name : allFilenames){
      s->PushString("$(HARDWARE_FOLDER)/%.*s ",UN(name));
    }

    TE_SetString("hardwareUnits",EndString(temp,s));
  }

  {  
    auto s = StartString(temp);
    for(String name : unitsGenerated){
        s->PushString("$(HARDWARE_FOLDER)/%.*s.v ",UN(name));
    }

    TE_SetString("moduleUnits",EndString(temp,s));
  }

  TE_SetNumber("addressSize",info->addressBits);
  TE_SetNumber("databusDataSize",globalOptions.databusDataSize);

  String traceType = {};
  if(globalDebug.outputVCD){
    traceType = "--trace";
    if(globalOptions.generateFSTFormat){
      traceType = "--trace-fst";
    }

    TE_SetString("traceType",traceType);
  }

  String res = TE_ProcessTemplate(out,META_MakefileTemplate);
  return res;
}

String GEN_OutputHardwareConfigurations(GEN_TopInfo* info,Arena* out){
#if 1
  TEMP_REGION(temp,out);

  FUDeclaration* decl = info->decl;

  VEmitter* m = StartVCode(temp);

  // VARS
  SYM_Expr configurationsBits = info->configTotalBitCount;
  int configurationAddressBits = info->memoryConfigDecisionBit;
  
  SYM_Print(configurationsBits);

  m->Timescale("1ns","1ps");

  m->Include("versat_defs.vh");

  m->Module("versat_configurations");
  {
    // TODO: We probably want to generate this from the parameters of the declaration, instead of hardcoded like this.
    m->ModuleParam("ADDR_W",32);
    m->ModuleParam("DATA_W",32);
    m->ModuleParam("AXI_ADDR_W",32);
    m->ModuleParam("AXI_DATA_W",globalOptions.databusDataSize);
    m->ModuleParam("LEN_W",20);
    m->ModuleParam("DELAY_W",7);

    if(!SYM_IsZeroValue(configurationsBits)){
      m->Output("config_data_o",configurationsBits);
      m->Reg("configdata",configurationsBits);

      if(globalOptions.shadowRegister){
        m->Reg("shadow_configdata",configurationsBits);
      }
    }

    m->Input("memoryMappedAddr");
    m->Input("data_write");

    m->Input("address",SYM_AddrW);
    m->Input("data_wstrb",SYM_DataStrobeW);
    m->Input("data_data",SYM_DataW);

    m->Input("change_config_pulse");

    for(GEN_Wire w : info->allConfigs){
      if(w.type & GEN_WireType_IS_STATIC){
        m->Output(w.name,w.size);
      }
    }

    m->Input("clk_i");
    m->Input("rst_i");

    m->Assign("config_data_o","configdata");

    for(GEN_Wire w : info->allConfigs){
      if(w.stage == VersatStage_COMPUTE || w.stage == VersatStage_WRITE){
        m->Reg(SF("Compute_%.*s",UN(w.name)),w.size);
      }
      if(w.stage == VersatStage_WRITE){
        m->Reg(SF("Write_%.*s",UN(w.name)),w.size);
      }
    }

    // TODO: Need to handle endianess
    auto EmitStrobe = [](VEmitter* m,String strobeWire,const char* leftReg,String leftIndexStart,const char* rightReg,int regSize){
      for(int i = 0; i < regSize; i += 8){
        int left = 8;
        if(i + left > regSize){
          left = regSize % 8;
        }
        
        m->If(SF("%.*s[%d]",UN(strobeWire),i/8));
        m->Set(SF("%s[(%.*s)+%d+:%d]",leftReg,UN(leftIndexStart),i,left),SF("%s[%d+:%d]",rightReg,i,left));
        m->EndIf();
      }
    };
    
    if(!SYM_IsZeroValue(configurationsBits)){
      m->Comment("Data to store to shadow config");
      m->Integer("i");
      m->AlwaysBlock("clk_i","rst_i");
      {
        m->If("rst_i");
        m->Set("shadow_configdata",0);
        m->ElseIf("data_write & !memoryMappedAddr");
        
        for(GEN_Wire w : info->allConfigs){
          if(w.type & GEN_WireType_IS_VERSAT){
            continue;
          }
          
          m->Comment(w.name);
          m->If(SF("address[%d:0] == %d",configurationAddressBits,w.address));
          
          String configStartExpr = SYM_Repr(w.startBit,temp);
          SYM_Expr size = w.size;
          
          if(!IsLiteral(size)){
            String repr = SYM_Repr(w.size,temp);

            // TODO: Need to handle endianess
            m->Loop("i = 0",SF("i < %.*s",UN(repr)),"i++");
            m->If(SF("data_wstrb[i/8]"));
            m->Set(SF("shadow_configdata[(%.*s)+i]",UN(configStartExpr)),SF("data_data[i]"));
            m->EndIf();
            m->EndLoop();
          } else {
            int litVal = LiteralValue(w.size);
            EmitStrobe(m,"data_wstrb","shadow_configdata",configStartExpr,"data_data",litVal);
          }
          
          m->EndIf();
        }
        m->EndIf();
      }
      m->EndBlock();
      
      m->Comment("Shadow config to config");
      m->AlwaysBlock("clk_i","rst_i");
      {
        m->If("rst_i");
        m->Set("configdata",0);
        m->ElseIf("change_config_pulse");
        for(GEN_Wire w : info->allConfigs){
          if(w.type & GEN_WireType_IS_VERSAT){
            continue;
          }

          String start = SYM_Repr(w.startBit,temp);
          String size = SYM_Repr(w.size,temp);
          
          if(w.stage == VersatStage_READ){
            m->Set(SF("configdata[(%.*s)+:%.*s]",UN(start),UN(size)),SF("shadow_configdata[(%.*s)+:%.*s]",UN(start),UN(size)));
          }
        }
        for(GEN_Wire w : info->allConfigs){
          if(w.type & GEN_WireType_IS_VERSAT){
            continue;
          }
          String start = SYM_Repr(w.startBit,temp);
          String size = SYM_Repr(w.size,temp);

          if(w.stage == VersatStage_COMPUTE || w.stage == VersatStage_WRITE){
            m->Set(SF("configdata[%.*s+:%.*s]",UN(start),UN(size)),SF("Compute_%.*s",UN(w.name)));
          }
        }
        m->EndIf();
      }
      m->EndBlock();
    }

    m->Comment("Compute");
    m->AlwaysBlock("clk_i","rst_i");
    {
      m->If("rst_i");
      for(GEN_Wire w : info->allConfigs){
        if(w.type & GEN_WireType_IS_VERSAT){
          continue;
        }
        if(w.stage == VersatStage_COMPUTE || w.stage == VersatStage_WRITE){
          m->Set(SF("Compute_%.*s",UN(w.name)),"0");
        }
      }
      m->ElseIf("change_config_pulse");
      for(GEN_Wire w : info->allConfigs){
        if(w.type & GEN_WireType_IS_VERSAT){
          continue;
        }
        String start = SYM_Repr(w.startBit,temp);
        String size = SYM_Repr(w.size,temp);
        if(w.stage == VersatStage_COMPUTE){
          m->Set(SF("Compute_%.*s",UN(w.name)),SF("shadow_configdata[(%.*s)+:%.*s]",UN(start),UN(size)));
        }
      }
      for(GEN_Wire w : info->allConfigs){
        if(w.type & GEN_WireType_IS_VERSAT){
          continue;
        }
        if(w.stage == VersatStage_WRITE){
          m->Set(SF("Compute_%.*s",UN(w.name)),SF("Write_%.*s",UN(w.name)));
        }
      }
      m->EndIf();
    }
    m->EndBlock();

    m->Comment("Write");
    m->AlwaysBlock("clk_i","rst_i");
    {
      m->If("rst_i");
      for(GEN_Wire w : info->allConfigs){
        if(w.type & GEN_WireType_IS_VERSAT){
          continue;
        }
        if(w.stage == VersatStage_WRITE){
          m->Set(SF("Write_%.*s",UN(w.name)),"0");
        }
      }
      m->ElseIf("change_config_pulse");
      for(GEN_Wire w : info->allConfigs){
        if(w.type & GEN_WireType_IS_VERSAT){
          continue;
        }
        String start = SYM_Repr(w.startBit,temp);
        String size = SYM_Repr(w.size,temp);
        if(w.stage == VersatStage_WRITE){
          m->Set(SF("Write_%.*s",UN(w.name)),SF("shadow_configdata[(%.*s)+:%.*s]",UN(start),UN(size)));
        }
      }
      m->EndIf();
    }
    m->EndBlock();

    for(GEN_Wire w : info->allConfigs){
      if(w.type & GEN_WireType_IS_VERSAT){
        continue;
      }
      if(w.type & GEN_WireType_IS_STATIC){
        String start = SYM_Repr(w.startBit,temp);
        String repr = SYM_Repr(w.size,temp);
        m->Assign(w.name,PushString(temp,"configdata[(%.*s)+:%.*s]",UN(start),UN(repr)));
      }
    }
  }
  m->EndModule();
  
  VAST* ast = EndVCode(m);
  StringBuilder* b = StartString(temp);
  Repr(ast,b);
  String content = EndString(out,b);
  return content;
#endif
}

String GEN_OutputHeader(GEN_TopInfo* info,Arena* out){
  TEMP_REGION(temp,out);

  FUDeclaration* decl = info->decl;
  COM_Unit* units = decl->fixedDelays.units.head;

  String typeName = decl->name;

  FREE_ARENA(CCode1);
  FREE_ARENA(CCode2);

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    COM_Unit topLevel = {};
    topLevel.name = typeName;
    topLevel.decl = decl;
    GEN_StructInfoList allStructs = {};
    GEN_StructInfo* configStruct = GEN_GenerateConfigStruct(&topLevel,&allStructs,temp);

    if(!configStruct){
      c->Struct(PushString(temp,"%.*s",UN(typeName)));
      c->EndStruct();
    }

    for(GEN_StructInfo* info = allStructs.head; info; info = info->next){
      c->Define(PushString(temp,"VERSAT_DEFINED_%.*s",UN(info->name)));
      c->Struct(PushString(temp,"%.*sConfig",UN(info->name)));
      for(GEN_StructElem* entry = info->elements; entry; entry = entry->next){
        if(entry->childs){
          c->Union();

          NOT_IMPLEMENTED("Missing merge stuff");
#if 0          
          for(SingleTypeStructElement typeAndName : entry.typeAndNames){
            if(typeAndName.arraySize > 1){
              c->Member(typeAndName.type,typeAndName.name,typeAndName.arraySize);
            } else {
              c->Member(typeAndName.type,typeAndName.name);
            }
          }
#endif

          c->EndStruct();
        } else {
          String typeName = entry->simpleTypename;
          if(Empty(typeName)){
            typeName = SF("%.*sConfig",UN(entry->type->name));
          }

          c->Member(typeName,entry->name,0);
        }
          
      }
      c->EndStruct();
      c->Line();
    }

    String content = PushASTRepr(c,temp);
    TE_SetString("configStructs",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    COM_Unit topLevel = {};
    topLevel.name = typeName;
    topLevel.decl = decl;
    GEN_StructInfoList allStructs = {};
    GEN_StructInfo* stateStruct = GEN_GenerateStateStruct(&topLevel,&allStructs,temp);

    if(!stateStruct){
      c->Struct(PushString(temp,"%.*s",UN(typeName)));
      c->EndStruct();
    }

    for(GEN_StructInfo* info = allStructs.head; info; info = info->next){
      c->Define(PushString(temp,"VERSAT_DEFINED_%.*s",UN(info->name)));
      c->Struct(PushString(temp,"%.*sState",UN(info->name)));
      for(GEN_StructElem* entry = info->elements; entry; entry = entry->next){
        if(entry->childs){
          c->Union();

          NOT_IMPLEMENTED("Missing merge stuff");
#if 0          
          for(SingleTypeStructElement typeAndName : entry.typeAndNames){
            if(typeAndName.arraySize > 1){
              c->Member(typeAndName.type,typeAndName.name,typeAndName.arraySize);
            } else {
              c->Member(typeAndName.type,typeAndName.name);
            }
          }
#endif

          c->EndStruct();
        } else {
          String typeName = entry->simpleTypename;
          if(Empty(typeName)){
            typeName = SF("%.*sState",UN(entry->type->name));
          }

          c->Member(typeName,entry->name,0);
        }
          
      }
      c->EndStruct();
      c->Line();
    }

    String content = PushASTRepr(c,temp);
    TE_SetString("stateStructs",content);
  }















  // Output user config functions ===============================================
#if 1
  {
    CEmitter* c = StartCCode(temp,CCode2);

    bool isMerge = false;
#if 0
    if(info.infos.size > 1){
      isMerge = true;
    }
#endif

    // Move this to meta stuff.
    auto ConfigVarTypeToName = [](COM_ArgType varType) -> String{
      FULL_SWITCH(varType){
        case COM_ArgType_NIL: Assert(false);
        case COM_ArgType_SIMPLE: return "int";
        case COM_ArgType_ADDR: return "void*";
      }
      NOT_POSSIBLE();
    };

    // Generate size function =====================================================
    
    //if(true){
    //for(int mergeIndex = 0; mergeIndex <  info.infos.size; mergeIndex++){
    //MergePartition part = info.infos[mergeIndex];
        
    for(COM_FunctionNode* node = decl->funcs; node; node = node->next){
      COM_Function* func = &node->v;
      
      if(func->type != COM_FunctionType_CONFIG){
        continue;
      }
      //if(!func->supportsSizeCalc){
      //  continue;
      //}

      // TODO: Only output this if used. Address gen with fixed addresses do not generate this ever.
      once {
        c->Struct("VersatVarSpec");
      c->Comment("Inputs, fill these with the min/max and the order of the variable");
      c->Member("int","min");
      c->Member("int","max");
      c->Member("int","order");
      c->Comment("Output: This value is the maximum amount that the variable can be for the amount of memory defined for the accelerator");
      c->Member("int","value");
      c->EndStruct();
    };
     
    String fullFunctionName = PushString(temp,"%.*s_%.*s_Size",UN(decl->name),UN(func->name));
    c->FunctionBlock("static inline int",fullFunctionName);

    auto list = PushList<String>(temp);

    for(COM_FuncParam param : func->params){
      if(!param.usedOnLoopExpressions){
        continue;
      }

      if(param.isDyn){
        String varName = PushString(temp,"%.*sSpec",UN(param.name));
        c->Argument("VersatVarSpec*",varName);
        *list->PushElem() = varName;
      } else if(param.type != COM_ArgType_ADDR){
        c->Argument(ConfigVarTypeToName(param.type),param.name);
      }
    }

    auto bothList = PushList<Pair<String,String>>(temp);

    for(COM_Stmt* stmt = func->stmts; stmt; stmt = stmt->next){
      if(stmt->type == COM_StmtType_ADDR_GEN){
        String symRepr = SYM_Repr(stmt->maxLoopSize,out);  //"10";//SYM_Repr(,temp);
        String maxSize = PushString(temp,"(1 << %.*s)",UN(symRepr));

        AddressAccess* access = stmt->access.access;
        SYM_Expr symb = GetLoopLinearSumTotalSize(access->internal);

        for(COM_FuncParam var : func->params){
          if(var.isDyn){
            SYM_Expr varSym = SYM_Var(SF("%.*sSpec->value",UN(var.name)));
                
            symb = SYM_Replace(symb,SYM_Var(var.name),varSym);
          }
        }
            
        String repr = SYM_Repr(symb,temp);
            
        *bothList->PushElem() = {maxSize,repr};
      }
    }

    auto d = StartString(temp);
    for(COM_FuncParam var : func->params){
      if(var.isDyn){
        d->PushString("    int %.*s = %.*sSpec->value;\n",UN(var.name),UN(var.name));
      }
    }
    auto specToVar = EndString(temp,d);

#if 0
    auto f = StartString(temp);
    for(ConfigComputation comp : func->extraComputations){
      f->PushString("    ");
      Repr(comp.cCode,f,false);
      f->PushString("\n");
    }
#endif
    String computations = {};//EndString(temp,f);

#if 1
    String varDeclareList = JoinStrings(list,",",temp);
    String varDeclare = PushString(temp,"{%.*s}",UN(varDeclareList));
      
    c->VarDeclare("VersatVarSpec*","buffer[]",varDeclare);
      
    auto b = StartString(temp);
    for(Pair<String,String> p : bothList){
      b->PushString(R"FOO(
    bytesUsed = VERSAT_MAX(bytesUsed,(%.*s) * sizeof(int) * 2);
    if(((%.*s) / (sizeof(int) * 2)) < (%.*s)){
      buffer[_VERSAT_index]->value -= 1;
      _VERSAT_index += 1;
      continue;    
    }
                     )FOO",UN(p.second),UN(p.first),UN(p.second));
    }
    String allStuff = EndString(temp,b);

    String tmpl = R"FOO(
  for(int i = 0; i < VERSAT_ARRAY_SIZE(buffer); i++){
    for(int j = i + 1; j < VERSAT_ARRAY_SIZE(buffer); j++){
      if(buffer[i]->order > buffer[j]->order){
        VersatVarSpec* temp = buffer[i];
        buffer[i] = buffer[j];
        buffer[j] = temp;
      } 
    }
  }

  for(int i = 0; i < VERSAT_ARRAY_SIZE(buffer); i++){
    buffer[i]->value = buffer[i]->min;
  }

  int _VERSAT_index = 0;
  int _VERSAT_totalSize = sizeof(int); // Assume that at worst we perform one single transfer. 
  while(_VERSAT_index < VERSAT_ARRAY_SIZE(buffer)){
    if(buffer[_VERSAT_index]->value >= buffer[_VERSAT_index]->max){
      _VERSAT_index += 1;
      continue;    
    }

    buffer[_VERSAT_index]->value += 1;

    int bytesUsed = 0;

@{specToVar}

@{computations}

    // NOTE: Pingpong cuts the usable memory in half. The reason for the '*2' logic
    @{allStuff}

    _VERSAT_totalSize = bytesUsed;
  }
                   )FOO";
#endif

#if 1
    TE_PushScope();

    TE_SetString("allStuff",allStuff);
    TE_SetString("specToVar",specToVar);
    TE_SetString("computations",computations);

    String inst = TE_ProcessTemplate(temp,tmpl);

    TE_PopScope();
    
    c->RawLine(inst);
#endif
      
    c->Return("_VERSAT_totalSize");

    c->EndBlock();
  }



















  //for(MergePartition part : info.infos){

    String mergeName = {}; //part.name;

#if 0

    // Output simulation functions if they exist ==================================
    for(COM_FunctionNode* funcNode = decl->funcs; funcNode; funcNode = funcNode->next){
      COM_Function* func = &funcNode->v;
      
      if(func->simLoops->type != ConfigSimStatementType_NIL){
        String fullFunctionName = PushString(temp,"%.*s_SIMULATE",UN(func->fullName));
        c->FunctionBlock("static inline void",fullFunctionName);
          
        for(ConfigVariable var : func->variables){
          c->Argument(ConfigVarTypeToName(var.type),var.name,var.arraySize);
        }

        c->VarDeclare("int","__VERSAT_INDEX","0");

        auto vars = PushList<String>(temp);
        auto GetVars = [c,vars](auto GetVars,ConfigSimStatement* head) -> void{
          for(ConfigSimStatement* ptr = head; ptr; ptr = ptr->next){
            FULL_SWITCH(ptr->type){
            case ConfigSimStatementType_LOOP:{
              GetVars(GetVars,ptr->child);
            } break;
            case ConfigSimStatementType_LHSName:{
              *vars->PushElem() = ptr->lhsName;
            } break;
            case ConfigSimStatementType_NIL:{
              // Nothing
            } break;
          }
          }
        };
        GetVars(GetVars,func->simLoops);

        Array<String> allVars = PushArray(temp,vars);
          
        {
          auto b = StartString(temp);

          b->PushString("versat_printf(\"%%15s");
          for(int i = 0; i < allVars.size; i++){
            b->PushString(",");
            b->PushString("%%15s");
          }

          b->PushString("\\n\",\"Index\"");
          for(String name : allVars){
            b->PushString(",\"");
            b->PushString(name);
            b->PushString("\"");
          }

          b->PushString(");");
          c->RawLine(EndString(temp,b));
        }

        auto Recurse = [c](auto Recurse,ConfigSimStatement* head) -> void{
          TEMP_REGION(temp,nullptr);

          bool anyVar = false;
          for(ConfigSimStatement* ptr = head; ptr; ptr = ptr->next){
            if(ptr->type == ConfigSimStatementType_LHSName){
              anyVar = true;
            }
          }
            
          for(ConfigSimStatement* ptr = head; ptr; ptr = ptr->next){
            FULL_SWITCH(ptr->type){
            case ConfigSimStatementType_LOOP:{
              String start = SYM_Repr(ptr->start,temp);
              String end = SYM_Repr(ptr->end,temp);
                
              c->ForEachBlock("int",ptr->varName,start,SF("%.*s < %.*s",UN(ptr->varName),UN(end)),SF("%.*s++",UN(ptr->varName)));
              Recurse(Recurse,ptr->child);
              c->EndBlock();
            } break;
            case ConfigSimStatementType_LHSName:{
              String expr = SYM_Repr(ptr->expression,temp);
              c->VarDeclare("int",ptr->lhsName,expr);
              anyVar = true;
            } break;
            case ConfigSimStatementType_NIL:{
              // Nothing
            } break;
          }
          }

          if(anyVar){
            auto b = StartString(temp);

            b->PushString("versat_printf(\"%%15d");
              
            for(ConfigSimStatement* ptr = head; ptr; ptr = ptr->next){
              if(ptr->type == ConfigSimStatementType_LHSName){
                b->PushString(",%%15d");
              }
            }

            b->PushString("\\n\",__VERSAT_INDEX");
            for(ConfigSimStatement* ptr = head; ptr; ptr = ptr->next){
              if(ptr->type == ConfigSimStatementType_LHSName){
                b->PushString(",");
                b->PushString(ptr->lhsName);
              }
            }
            b->PushString(");");

            c->RawLine(EndString(temp,b));
            c->RawLine("__VERSAT_INDEX++;");
          }
            
        }; 
          
        Recurse(Recurse,func->simLoops);

        c->EndBlock();
      }
    }

#endif

    // Output configuration functions =============================================
    for(COM_FunctionNode* funcNode = decl->funcs; funcNode; funcNode = funcNode->next){
      COM_Function* func = &funcNode->v;
      bool isState = (func->type == COM_FunctionType_STATE);

      String returnType = "void";
      if(!Empty(func->structToReturn)){
        returnType = PushString(out,"%.*s_%.*s",UN(decl->name),UN(func->structToReturn));
      }

      if(isState){
        c->Struct(returnType);
        for(COM_Stmt* stmt = func->stmts; stmt; stmt = stmt->next){
          c->VarDeclare("iptr",stmt->lhs);
        }        
        c->EndStruct();
      }

      String fullFunctionName = PushString(temp,"%.*s_%.*s",UN(decl->name),UN(func->name));
      c->FunctionBlock(SF("static inline %.*s",UN(returnType)),fullFunctionName);

      for(COM_FuncParam var : func->params){
        c->Argument(ConfigVarTypeToName(var.type),var.name);
      }

#if 0
      if(func->debug){
        String str = PushString(temp,"versat_printf(\"[DEBUG] [%.*s]\\n\")",UN(func->fullName));
        c->Statement(str);
          
        for(ConfigVariable var : func->variables){
          String printVar = PushString(temp,"versat_printf(\"  %.*s : %%d\\n\",%.*s)",UN(var.name),UN(var.name));
          c->Statement(printVar);
        }
      }
#endif
        
      String assignStarter = "accelConfig";

#if 1
      if(isState){
        assignStarter = "accelState";
      }
#endif
        
      // Because of merge, need to generate the config pointer that allow us to access the named members directly.
#if 0
      if(isMerge){
        String stmt = {};
        if(isState){
          assignStarter = "state";
          stmt = PushString(temp,"volatile %.*sState* state = &accelState->%.*s",UN(mergeName),UN(mergeName));
          if(info.states > 0){
            c->Statement(stmt);
          }
        } else {
          assignStarter = "config";
          stmt = PushString(temp,"volatile %.*sConfig* config = &accelConfig->%.*s",UN(mergeName),UN(mergeName));
          c->Statement(stmt);
        }
      }
#endif

#if 0
      for(ConfigComputation comp : func->extraComputations){
        c->InsertCode(comp.cCode);
      }
#endif
        
      FULL_SWITCH(func->type){
      case COM_FunctionType_NIL:{
        Assert(false);
      } break;
      case COM_FunctionType_MEM:{
        for(COM_Stmt* stmt = func->stmts; stmt; stmt = stmt->next){
          String sizeExpr = SYM_Repr(stmt->rhsExpr,temp);
          
          FULL_SWITCH(stmt->dir){
          case Direction_NONE: Assert(false); break;
          case Direction_READ: {
            String expr = PushString(temp,"VersatMemoryCopy(TOP_%.*s,%.*s,(%.*s) * sizeof(int));",UN(stmt->lhs),UN(stmt->rhs),UN(sizeExpr));
            c->RawLine(expr);
          } break;
          case Direction_WRITE: {
            String expr = PushString(temp,"VersatMemoryCopy(%.*s,TOP_%.*s,(%.*s) * sizeof(int));",UN(stmt->rhs),UN(stmt->lhs),UN(sizeExpr));
            c->RawLine(expr);
          } break;
        }
        }
      } break;
      case COM_FunctionType_STATE:{
        c->VarDeclare(returnType,"res","{}");

        for(COM_Stmt* stmt = func->stmts; stmt; stmt = stmt->next){
          Assert(stmt->type == COM_StmtType_ASSIGN);
          String lhs = PushString(temp,"res.%.*s",UN(stmt->lhs));
          String rhs = PushString(temp,"%.*s->%.*s",UN(assignStarter),UN(stmt->rhs));
          c->Assignment(lhs,rhs);
        }        

        c->Return("res");
      } break;

      case COM_FunctionType_CONFIG:{
        for(COM_Stmt* stmt = func->stmts; stmt; stmt = stmt->next){
          switch(stmt->type){
            case COM_StmtType_NIL: Assert(false);
            case COM_StmtType_ASSIGN: {
              String lhs = PushString(temp,"%.*s->%.*s",UN(assignStarter),UN(stmt->lhs));
              String repr = stmt->rhs;

              c->Assignment(lhs,repr);
            } break;
            // MARK
            
            case COM_StmtType_ADDR_GEN: {
              c->RawLine("{");
             
              AccessAndType access = stmt->access;
              AddressGenInst inst = access.inst;
              
              String fullLhs = stmt->lhs; //HIER_GetFullName(stmt->lhs,".",temp);
              
              bool pushStartToExt = (inst.type == AddressGenType_READ);

              String lhs = PushString(temp,"%.*s->%.*s",UN(assignStarter),UN(fullLhs));

              auto Recurse = [lhs,c,pushStartToExt,temp](auto Recurse,CodeNode* top) -> void{
                SYM_Expr startExpr = SYM_0;
                if(pushStartToExt){
                  for(CodeNode* ptr = top; ptr; ptr = ptr->next){
                    if(ptr->name == "start"){
                      startExpr = ptr->expr;
                    }
                  }
                }
                  
                for(CodeNode* ptr = top; ptr; ptr = ptr->next){
                  String repr = SYM_Repr(ptr->expr,temp);

                  FULL_SWITCH(ptr->type){
                  case CodeNodeType_EMPTY:{
                    Recurse(Recurse,ptr->child);
                  } break;
                  case CodeNodeType_IF:{
                    c->If(repr);
                    Recurse(Recurse,ptr->child);
                    c->EndIf();
                  } break;
                  case CodeNodeType_COMMENT:{
                    c->Comment(ptr->name);
                  } break;
                  case CodeNodeType_DECLARE:{
                    String repr = SYM_Repr(ptr->expr,temp);
                    c->VarDeclare("int",ptr->name,repr);
                  } break;
                  case CodeNodeType_ASSIGN:{
                    if(pushStartToExt && ptr->name == "start"){
                      continue;
                    }
                      
                    String fullName = PushString(temp,"%.*s.%.*s",UN(lhs),UN(ptr->name));

                    if(pushStartToExt && ptr->name == "ext_addr"){
                      if(!Equal(startExpr,SYM_0)){
                        String startExprRepr = SYM_Repr(startExpr,temp);
                        repr = PushString(temp,"(iptr) (((float*) %.*s) + (%.*s))",UN(repr),UN(startExprRepr));
                      } else {
                        repr = PushString(temp,"(iptr) (%.*s)",UN(repr));
                      }
                    }
                    if(ptr->name == "length"){
                      repr = PushString(temp,"(%.*s) * sizeof(float)",UN(repr));
                    }
                    if(ptr->name == "addr_shift"){
                      repr = PushString(temp,"(%.*s) * sizeof(float)",UN(repr));
                    }

                    c->Assignment(fullName,repr);
                  } break;
                }
                }
              };

              InstantiateOptions options = {};
              options.type = inst.type;
              options.extVarName = stmt->rhs; //assign.pointerVarName;
              options.memPort = access.port;
              options.dir = access.dir;
              
              CodeNode* top = EmitStatements(access,temp,options);
              Recurse(Recurse,top);

              c->RawLine("}");

            } break;
            case COM_StmtType_MEM_COPY: {
              NOT_IMPLEMENTED();
            } break;
          }
        }

#if 0
        for(ConfigStuff assign : func->stuff){
          FULL_SWITCH(assign.type){
          case ConfigStuffType_ASSIGNMENT:{
            String lhs = PushString(temp,"%.*s->%.*s",UN(assignStarter),UN(assign.assign.lhs));
            SYM_Expr rhs = assign.assign.rhs;
            String repr = SYM_Repr(rhs,temp);

            c->Assignment(lhs,repr);
          } break;
          case ConfigStuffType_ADDRESS_GEN:{
            c->RawLine("{");
             
            AccessAndType access = assign.access;
            AddressGenInst inst = access.inst;
              
            String fullLhs = HIER_GetFullName(assign.lhs,".",temp);
              
            bool pushStartToExt = (inst.type == AddressGenType_READ);

            String lhs = PushString(temp,"%.*s->%.*s",UN(assignStarter),UN(fullLhs));

            auto Recurse = [lhs,c,pushStartToExt,temp](auto Recurse,CodeNode* top) -> void{
              SYM_Expr startExpr = SYM_0;
              if(pushStartToExt){
                for(CodeNode* ptr = top; ptr; ptr = ptr->next){
                  if(ptr->name == "start"){
                    startExpr = ptr->expr;
                  }
                }
              }
                  
              for(CodeNode* ptr = top; ptr; ptr = ptr->next){
                String repr = SYM_Repr(ptr->expr,temp);

                FULL_SWITCH(ptr->type){
                case CodeNodeType_EMPTY:{
                  Recurse(Recurse,ptr->child);
                } break;
                case CodeNodeType_IF:{
                  c->If(repr);
                  Recurse(Recurse,ptr->child);
                  c->EndIf();
                } break;
                case CodeNodeType_COMMENT:{
                  c->Comment(ptr->name);
                } break;
                case CodeNodeType_DECLARE:{
                  String repr = SYM_Repr(ptr->expr,temp);
                  c->VarDeclare("int",ptr->name,repr);
                } break;
                case CodeNodeType_ASSIGN:{
                  if(pushStartToExt && ptr->name == "start"){
                    continue;
                  }
                      
                  String fullName = PushString(temp,"%.*s.%.*s",UN(lhs),UN(ptr->name));

                  if(pushStartToExt && ptr->name == "ext_addr"){
                    if(!Equal(startExpr,SYM_0)){
                      String startExprRepr = SYM_Repr(startExpr,temp);
                      repr = PushString(temp,"(iptr) (((float*) %.*s) + (%.*s))",UN(repr),UN(startExprRepr));
                    } else {
                      repr = PushString(temp,"(iptr) (%.*s)",UN(repr));
                    }
                  }
                  if(ptr->name == "length"){
                    repr = PushString(temp,"(%.*s) * sizeof(float)",UN(repr));
                  }
                  if(ptr->name == "addr_shift"){
                    repr = PushString(temp,"(%.*s) * sizeof(float)",UN(repr));
                  }

                  c->Assignment(fullName,repr);
                } break;
              }
              }
            };
              
            InstantiateOptions options = {};
            options.type = inst.type;
            options.extVarName = assign.pointerVarName;
            options.memPort = access.port;
            options.dir = access.dir;

            CodeNode* top = EmitStatements(access,temp,options);
            Recurse(Recurse,top);

            c->RawLine("}");

          } break;
          case ConfigStuffType_MEMORY_TRANSFER:{
            NOT_POSSIBLE();
          } break;
        }
        }
#endif
      } break;
    }

      c->EndBlock();
    }
  //}

  String content = PushASTRepr(c,temp);
  TE_SetString("userConfigFunctions",content);
  }
#endif
  
#if 0
  Array<String> names = Extract(info.infos,temp,&MergePartition::name);
  Array<Array<MuxInfo>> muxInfo = CalculateMuxInformation(&iter,temp);
#endif

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    for(GEN_MMInfo e : info->memoryMapped){
      c->Define(SF("TOP_%.*s",UN(e.addrName)),PushString(temp,"((void*) (versat_base + memMappedStart + %d))",e.addrStart));
    }
      
    String content = PushASTRepr(c,temp);
    TE_SetString("memMappedAddresses",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    DEBUG_BREAK();
    c->VarBlock();
    for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
      for(int i = 0; i < ptr->extraDelay.size; i++){
        if(i < ptr->decl->numberInputDelays){
          c->Elem(PushString(temp,"0x%x",ptr->extraDelay[i]));
        }
      }
    }
    for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
      for(int i = 0; i < ptr->extraDelay.size; i++){
        if(i >= ptr->decl->numberInputDelays){
          c->Elem(PushString(temp,"0x%x",ptr->extraDelay[i]));
        }
      }
    }

    String content = PushASTRepr(c,temp);
    TE_SetString("delayBlock",content);
  }

#if 0
  // Accelerator header
  auto arr = StartGrowableArray<int>(temp);
  for(InstanceInfo& t : info.infos[0].info){
    if(!t.isComposite){
      for(int d : t.extraDelay){
        *arr.PushElem() = d;
      }
    }
  }
  Array<int> delays = EndArray(arr);

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    c->VarBlock();
    for(auto d : delays){
      c->Elem(PushString(temp,"0x%x",d));
    }
    c->EndBlock();

    String content = PushASTRepr(c,temp);
    TE_SetString("delayBlock",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    for(auto elem : allStaticsVerilatorSide){
      c->Define(PushString(temp,"ACCEL_%.*s",UN(elem.name)),PushString(temp,"accelStatic->%.*s",UN(elem.name)));
    }
      
    String content = PushASTRepr(c,temp);
    TE_SetString("allStaticDefines",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    // TODO: Check what is going on with variable delay, current tests all pass even if this is set to false.
    bool hasVariableDelay = false;

#if 0
    for(int i = 0; i <  info.infos.size; i++){
      for(AccelInfoIterator iter = StartIteration(&info,i); iter.IsValid(); iter = iter.Next()){
        InstanceInfo* info = iter.CurrentUnit();

        if(info->specialType == SpecialUnitType_VARIABLE_BUFFER){
          hasVariableDelay = true;
        }
      }
    }

    if(hasVariableDelay){
      for(int i = 0; i <  info.infos.size; i++){
        c->ArrayDeclareBlock("unsigned int",SF("bufferValues_%d",i),true);

        for(AccelInfoIterator iter = StartIteration(&info,i); iter.IsValid(); iter = iter.Step()){
          InstanceInfo* info = iter.CurrentUnit();

          if(info->specialType != SpecialUnitType_VARIABLE_BUFFER){
            continue;
          }

          c->Elem(PushString(temp,"0x%x",info->variableBufferDelay));
        }

        c->EndBlock();
      }

      c->ArrayDeclareBlock("unsigned int*","bufferValues",true);

      for(int i = 0; i < info.infos.size; i++){
        c->Elem(SF("bufferValues_%d",i));
      }
        
      c->EndBlock();
    }
#endif

    if(names.size > 1){
      for(int i = 0; i <  allDelays.size; i++){
        auto delayArray  =  allDelays[i];
        c->ArrayDeclareBlock("unsigned int",SF("delayBuffer_%d",i),true);

        for(auto d : delayArray){
          c->Elem(PushString(temp,"0x%x",d));
        }

        c->EndBlock();
      }

      c->ArrayDeclareBlock("unsigned int*","delayBuffers",true);

      for(int i = 0; i < names.size; i++){
        c->Elem(SF("delayBuffer_%d",i));
      }
        
      c->EndBlock();

      c->Enum("MergeType");
      for(int i = 0; i <  names.size; i++){
        auto name  =  names[i];
        c->EnumMember(SF("MergeType_%.*s",UN(name)),SF("%d",i));
      }
      c->EndEnum();

      c->FunctionBlock("static inline void","ActivateMergedAccelerator");
      c->Argument("MergeType","type");

      c->Assignment("int asInt","(int) type");

      if(muxInfo.size > 0){
        c->SwitchBlock("type");
        for(int i = 0 ; i < names.size; i++){
          c->CaseBlock(SF("MergeType_%.*s",UN(names[i])));

          for(auto info : muxInfo[i]){
            c->Assignment(SF("accelConfig->%.*s.sel",UN(info.name)),SF("%d",info.val));
          }
          c->EndBlock();
        }
        c->EndBlock();
      }

      c->RawLine("VersatLoadDelay(delayBuffers[asInt]);");

#if 0
      if(hasVariableDelay){
        int index = 0;
        for(AccelInfoIterator iter = StartIteration(&info); iter.IsValid(); iter = iter.Step()){
          InstanceInfo* info = iter.CurrentUnit();

          if(info->specialType != SpecialUnitType_VARIABLE_BUFFER){
            continue;
          }

          // Just to make sure. Variable buffers are Versat units so we can just assume that this holds.
          Assert(info->configs.size == 1);

          String name = GetStaticWireFullName(info,info->configs[0],temp);
          c->Assignment(PushString(temp,"ACCEL_%.*s",UN(name)),SF("bufferValues[asInt][%d]",index++));
        }
      }
#endif
      
      c->EndBlock();
    }
      
    String content = PushASTRepr(c,temp);
    TE_SetString("mergeStuff",content);
  }
#endif

  TE_SetNumber("databusDataSize",globalOptions.databusDataSize);
  TE_SetHex("memMappedStart",1 << (info->addressBits - 1));
  TE_SetHex("versatAddressSpace",1 << info->addressBits);
  TE_SetHex("staticStart",info->staticStart);
  TE_SetHex("configStart",info->configStart);
  TE_SetHex("stateStart",info->registers.size);
  TE_SetBool("useDMA",globalOptions.useDMA);
  TE_SetString("typeName",decl->name);
  TE_SetNumber("nConfigs",info->totalConfigs);
  TE_SetNumber("nStates",info->stateCount);

  // TODO
  TE_SetHex("delayStart",info->delayStart);

  String res = TE_ProcessTemplate(out,META_HeaderTemplate,true);
  return res;
}

String GEN_OutputVerilatorHardwareWrapper(GEN_TopInfo* info,Arena* out){
  TEMP_REGION(temp,out);

  // Same interface as top unit.
  FUDeclaration* module = info->decl;
  FUDeclaration* decl = module;

  FREE_ARENA(emitterArena);
  VEmitter* m = StartVCode(emitterArena);
  m->Timescale("1ns","1ps");

  m->Module("Versat_verilator_wrapper");
#if 0
  for(DECL_Param p : decl->parameters){
    m->ModuleParam(p.name,p.val);
  }
#endif
  m->ModuleParam("ADDR_W",32);
  m->ModuleParam("DATA_W",32);
  m->ModuleParam("LEN_W",32);
  m->ModuleParam("DELAY_W",32);
  m->ModuleParam("AXI_DATA_W",0);
  m->ModuleParam("AXI_ADDR_W",0);

  // NOTE: Verilator wrapper is currently hardcoded to need these interfaces.
  if(true || decl->singleInterfaces & DECL_SingleInterface_RUNNING){
    m->Input("running");
  }
  if(true || decl->singleInterfaces & DECL_SingleInterface_RUN){
    m->Input("run");
  }
  if(true || decl->singleInterfaces & DECL_SingleInterface_CLK){
    m->Input("clk");
  }
  if(true || decl->singleInterfaces & DECL_SingleInterface_RESET){
    m->Input("rst");
  }
  if(module->singleInterfaces & DECL_SingleInterface_LOOP){
    m->Input("signal_loop");
  }
  if(module->numberDones){
    m->Output("done");
  }
  
  for(int i = 0; i < module->inputs.size; i++){
    m->InputIndexed("in%d",i,SYM_DataW);
  }

  for(int i = 0; i < module->outputs.size; i++){
    m->OutputIndexed("out%d",i,SYM_DataW);
  }

  if(module->configs.size) m->Blank();
  for(Wire w : module->configs){
    m->Input(w.name,w.sizeExpr);
  }

  for(DECL_StaticWire w : module->statics){
    String repr = GEN_StaticName(w,temp);
    m->Input(repr,w.wire.sizeExpr);
  }

  for(Wire w : module->states){
    m->Output(w.name,w.sizeExpr);
  }

  for(int i = 0; i < module->numberInputDelays; i++){
    m->InputIndexed("delay_in%d",i,SYM_DelayW);
  }
  for(int i = 0; i < module->numberOutputDelays; i++){
    m->InputIndexed("delay_out%d",i,SYM_DelayW);
  }

  // Databus interface
  for(int i = 0; i < module->numberDatabus; i++){
    m->InputIndexed("databus_ready_%d",i);
    m->OutputIndexed("databus_valid_%d",i);
    m->OutputIndexed("databus_addr_%d",i,SYM_AxiAddrW);
    m->InputIndexed("databus_rdata_%d",i,SYM_AxiDataW);
    m->OutputIndexed("databus_wdata_%d",i,SYM_AxiDataW);
    m->OutputIndexed("databus_wstrb_%d",i,SYM_AxiStrobeW);
    m->OutputIndexed("databus_len_%d",i,SYM_LenW);
    m->InputIndexed("databus_last_%d",i);
  }

  if(module->memoryMapped.size){
    for(int i = 0; i < module->memoryMapped.size; i++){
      m->InputIndexed("unit_valid_%d",i);
    }

    m->Input("addr",SYM_AddrW);
    m->Input("wstrb",SYM_DataStrobeW);
    m->Input("wdata",SYM_DataW);
    m->Output("rvalid");
    m->Output("rdata","DATA_W");
  }

  for(int i = 0; i < decl->externalMemory.size; i++){
    HW_Instance ext = decl->externalMemory[i];
    SYM_Expr addrSize = HW_GetAddrSize(ext,0);
    Assert(!IsNil(addrSize));

    if(HW_IsMemoryDP(ext.inter)){
      SYM_Expr data0Size = HW_GetDataSize(ext,0);
      SYM_Expr data1Size = HW_GetDataSize(ext,1);
      SYM_Expr dataSize[2] = {data0Size,data1Size};
      Assert(!IsNil(data0Size) && !IsNil(data1Size));

      for(int k = 0; k < 2; k++){
        m->Wire(SF("ext_dp_addr_%d_port_%d",i,k),addrSize);
        m->Wire(SF("ext_dp_out_%d_port_%d",i,k),dataSize[k]);
        m->Wire(SF("ext_dp_in_%d_port_%d",i,k),dataSize[k]);
        m->Wire(SF("ext_dp_enable_%d_port_%d",i,k));
        m->Wire(SF("ext_dp_write_%d_port_%d",i,k));
      }
    }

    if(HW_IsMemory2P(ext.inter)){
      SYM_Expr dataOutSize = HW_GetDataSize(ext,0,Direction_OUTPUT);
      SYM_Expr dataInSize = HW_GetDataSize(ext,0,Direction_INPUT);
      Assert(!IsNil(dataOutSize) && !IsNil(dataInSize));

      m->Wire(SF("ext_2p_addr_out_%d",i),addrSize);
      m->Wire(SF("ext_2p_addr_in_%d",i),addrSize);
      m->Wire(SF("ext_2p_data_in_%d",i),dataInSize);
      m->Wire(SF("ext_2p_data_out_%d",i),dataOutSize);
      m->Wire(SF("ext_2p_write_%d",i));
      m->Wire(SF("ext_2p_read_%d",i));
    }
  }

  for(int i = 0; i < decl->externalMemory.size; i++){
    HW_Instance ext = decl->externalMemory[i];
    // nocheckin
    //SYM_Expr addrSize = SYM_16;
    SYM_Expr addrSize = HW_GetAddrSize(ext,0);
    Assert(!IsNil(addrSize));

    if(HW_IsMemoryDP(ext.inter)){
      SYM_Expr data0Size = HW_GetDataSize(ext,0);
      SYM_Expr data1Size = HW_GetDataSize(ext,1);
      Assert(!IsNil(data0Size) && !IsNil(data1Size));

      m->StartInstance(DP_ModuleName,SF("ext_dp_%d",i));
      m->InstanceParam("A_DATA_W",data0Size);
      m->InstanceParam("B_DATA_W",data1Size);
      m->InstanceParam("ADDR_W",addrSize);

      m->PortConnect("dinA_i",SF("ext_dp_out_%d_port_0",i));
      m->PortConnect("addrA_i",SF("ext_dp_addr_%d_port_0",i));
      m->PortConnect("enA_i",SF("ext_dp_enable_%d_port_0",i));
      m->PortConnect("weA_i",SF("ext_dp_write_%d_port_0",i));
      m->PortConnect("doutA_o",SF("ext_dp_in_%d_port_0",i));

      m->PortConnect("dinB_i",SF("ext_dp_out_%d_port_1",i));
      m->PortConnect("addrB_i",SF("ext_dp_addr_%d_port_1",i));
      m->PortConnect("enB_i",SF("ext_dp_enable_%d_port_1",i));
      m->PortConnect("weB_i",SF("ext_dp_write_%d_port_1",i));
      m->PortConnect("doutB_o",SF("ext_dp_in_%d_port_1",i));

      m->PortConnect("clk_i","clk");
      m->EndInstance();
    }
    if(HW_IsMemory2P(ext.inter)){
      SYM_Expr dataOutSize = HW_GetDataSize(ext,0,Direction_OUTPUT);
      SYM_Expr dataInSize = HW_GetDataSize(ext,0,Direction_INPUT);
        
      m->StartInstance(TWO_P_ModuleName,SF("ext_2p_%d",i));
      m->InstanceParam("W_DATA_W",dataOutSize);
      m->InstanceParam("R_DATA_W",dataInSize);
      m->InstanceParam("ADDR_W",addrSize);

      m->PortConnect("w_en_i",SF("ext_2p_write_%d",i));
      m->PortConnect("w_addr_i",SF("ext_2p_addr_out_%d",i));
      m->PortConnect("w_data_i",SF("ext_2p_data_out_%d",i));

      m->PortConnect("r_en_i",SF("ext_2p_read_%d",i));
      m->PortConnect("r_addr_i",SF("ext_2p_addr_in_%d",i));
      m->PortConnect("r_data_o",SF("ext_2p_data_in_%d",i));

      m->PortConnect("clk_i","clk");
      m->EndInstance();
    }
  }

  m->StartInstance(module->name,"TOP");

  for(DECL_Param p : decl->parameters){
    // TODO: nocheckin HACK
    if(p.name == "ADDR_W"){
      m->InstanceParam(p.name,SYM_AddrW);
    } else {
      m->InstanceParam(p.name,p.val);
    }
  }

  // Instance
  if(decl->singleInterfaces & DECL_SingleInterface_LOOP){
    m->PortConnect("signal_loop","signal_loop");
  }
  if(decl->singleInterfaces & DECL_SingleInterface_RUNNING){
    m->PortConnect("running","running");
  }
  if(decl->singleInterfaces & DECL_SingleInterface_RUN){
    m->PortConnect("run","run");
  }
  if(decl->singleInterfaces & DECL_SingleInterface_DONE){
    m->PortConnect("done",SF("done"));
  }
  if(decl->singleInterfaces & DECL_SingleInterface_CLK){
    m->PortConnect("clk","clk");
  }
  if(decl->singleInterfaces & DECL_SingleInterface_RESET){
    m->PortConnect("rst","rst");
  }
  
  for(int i = 0; i < module->inputs.size; i++){
    m->PortConnectIndexed("in%d",i,SF("in%d",i));
  }

  for(int i = 0; i < module->outputs.size; i++){
    m->PortConnectIndexed("out%d",i,SF("out%d",i));
  }

  if(module->configs.size) m->Blank();
  for(Wire w : module->configs){
    m->PortConnect(w.name,w.name);
  }
  
  for(DECL_StaticWire w : decl->statics){
    String name = GEN_StaticName(w,temp);
    m->PortConnect(name,name);
  }

  for(Wire w : module->states){
    m->PortConnect(w.name,w.name);
  }

  for(int i = 0; i < module->numberInputDelays; i++){
    m->PortConnectIndexed("delay_in%d",i,SF("delay_in%d",i));
  }
  for(int i = 0; i < module->numberOutputDelays; i++){
    m->PortConnectIndexed("delay_out%d",i,SF("delay_out%d",i));
  }

  // Databus interface
  int ioSeen = 0;
  for(int i = 0; i < decl->numberDatabus; i++){
    m->PortConnectIndexed("databus_ready_%d",i,"databus_ready_%d",ioSeen);
    m->PortConnectIndexed("databus_valid_%d",i,"databus_valid_%d",ioSeen);
    m->PortConnectIndexed("databus_addr_%d",i,"databus_addr_%d",ioSeen);
    m->PortConnectIndexed("databus_rdata_%d",i,"databus_rdata_%d",ioSeen);
    m->PortConnectIndexed("databus_wdata_%d",i,"databus_wdata_%d",ioSeen);
    m->PortConnectIndexed("databus_wstrb_%d",i,"databus_wstrb_%d",ioSeen);
    m->PortConnectIndexed("databus_len_%d",i,"databus_len_%d",ioSeen);
    m->PortConnectIndexed("databus_last_%d",i,"databus_last_%d",ioSeen);

    ioSeen += 1;
  }

  if(module->memoryMapped.size){
    for(int i = 0; i < module->memoryMapped.size; i++){
    }
  }

  if(module->memoryMapped.size > 0){
    m->PortConnect("wstrb","wstrb");
    m->PortConnect("addr",SF("addr"));
    m->PortConnect("rdata",SF("rdata"));
    m->PortConnect("rvalid",SF("rvalid"));
    m->PortConnect("wdata","wdata");
        
    for(int i = 0; i < decl->memoryMapped.size; i++){
      m->PortConnectIndexed("unit_valid_%d",i,SF("unit_valid_%d",i));
    }
  }

  for(int i = 0; i < module->externalMemory.size; i++){
    HW_Instance& ext = module->externalMemory[i];
    int externalSeen = i;
    if(ext.inter == &HW_Default::HW_DP){
      m->PortConnectIndexed("ext_dp_addr_%d_port_0",i,"ext_dp_addr_%d_port_0",externalSeen);
      m->PortConnectIndexed("ext_dp_out_%d_port_0",i,"ext_dp_out_%d_port_0",externalSeen);
      m->PortConnectIndexed("ext_dp_in_%d_port_0",i,"ext_dp_in_%d_port_0",externalSeen);
      m->PortConnectIndexed("ext_dp_enable_%d_port_0",i,"ext_dp_enable_%d_port_0",externalSeen);
      m->PortConnectIndexed("ext_dp_write_%d_port_0",i,"ext_dp_write_%d_port_0",externalSeen);

      m->PortConnectIndexed("ext_dp_addr_%d_port_1",i,"ext_dp_addr_%d_port_1",externalSeen);
      m->PortConnectIndexed("ext_dp_out_%d_port_1",i,"ext_dp_out_%d_port_1",externalSeen);
      m->PortConnectIndexed("ext_dp_in_%d_port_1",i,"ext_dp_in_%d_port_1",externalSeen);
      m->PortConnectIndexed("ext_dp_enable_%d_port_1",i,"ext_dp_enable_%d_port_1",externalSeen);
      m->PortConnectIndexed("ext_dp_write_%d_port_1",i,"ext_dp_write_%d_port_1",externalSeen);
    } else {
      m->PortConnectIndexed("ext_2p_addr_out_%d",i,"ext_2p_addr_out_%d",externalSeen);
      m->PortConnectIndexed("ext_2p_addr_in_%d",i,"ext_2p_addr_in_%d",externalSeen);
      m->PortConnectIndexed("ext_2p_write_%d",i,"ext_2p_write_%d",externalSeen);
      m->PortConnectIndexed("ext_2p_read_%d",i,"ext_2p_read_%d",externalSeen);
      m->PortConnectIndexed("ext_2p_data_in_%d",i,"ext_2p_data_in_%d",externalSeen);
      m->PortConnectIndexed("ext_2p_data_out_%d",i,"ext_2p_data_out_%d",externalSeen);
    }
  }

  m->EndInstance();

  m->EndModule();
  
  VAST* ast = EndVCode(m);
  StringBuilder* b = StartString(temp);
  Repr(ast,b);
  String content = EndString(out,b);

  return content;
}

String GEN_OutputVerilatorSoftwareWrapper(GEN_TopInfo* info,Arena* out){
  TEMP_REGION(temp,out);

  FUDeclaration* decl = info->decl;

  struct WireExtra{
    String name;
    Wire w;
    String source;
  };

  auto build = PushList<WireExtra>(temp);
  for(COM_Unit* unit = decl->baseGraph.units.head; unit; unit = unit->next){
    for(Wire config : unit->decl->configs){
      WireExtra* ptr = build->PushElem();
      ptr->w = config;
      ptr->name = PushString(temp,"%.*s_%.*s",UN(unit->name),UN(config.name));
      ptr->source = "config->";
    }
  }
  for(DECL_StaticWire w : decl->statics){
    WireExtra* ptr = build->PushElem();
    ptr->w = w.wire;
    ptr->source = "statics->";
  }
  Array<WireExtra> allConfigsVerilatorSide = PushArray(temp,build);

  TE_SetNumber("nIOs",info->numberDatabus);

  FREE_ARENA(CCode1);
  FREE_ARENA(CCode2);

  {
    CEmitter* c = StartCCode(CCode1,CCode2);
    c->Struct("AcceleratorConfig");

    for(WireExtra e : allConfigsVerilatorSide){
      c->Member("iptr",e.name);
    }
      
    c->EndStruct();
    String content = PushASTRepr(c,temp);
    TE_SetString("acceleratorConfig",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);
    c->Struct("AcceleratorState");

    for(Wire e : decl->states){
      c->Member("iptr",e.name);
    }

    c->EndStruct();
    String content = PushASTRepr(c,temp);
    TE_SetString("acceleratorState",content);
  }
  

#if 1
  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    // TODO: BAD
    c->RawLine("AcceleratorConfig* config = (AcceleratorConfig*) &configBuffer;\n");

#if 0
    EmitDisableReadsAndWrites(c,"config",&info);
#endif

    String content = PushASTRepr(c,temp);
    TE_SetString("disableReadsAndWrites",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    if(globalDebug.outputVCD){
      c->Define("TRACE");
    }
      
    if(globalOptions.generateFSTFormat){
      c->Define("TRACE_FST");
    } else {
      c->Define("TRACE_VCD");
    }

    if(decl->singleInterfaces & DECL_SingleInterface_LOOP){
      c->Define("SIGNAL_LOOP");
    }

    if(true){
      c->Define("SIMULATE_LOOPS");
    }
      
    String content = PushASTRepr(c,temp);
    TE_SetString("defines",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    int index = 0;
    for(int i = 0; i < decl->numberInputDelays; i++){
      c->Assignment(PushString(temp,"self->delay_in%d",i),PushString(temp,"delayBuffer[%d]",index++));
    }
    for(int i = 0; i < decl->numberOutputDelays; i++){
      c->Assignment(PushString(temp,"self->delay_out%d",i),PushString(temp,"delayBuffer[%d]",index++));
    }
    c->RawLine("self->eval();");
        
    String content = PushASTRepr(c,temp,false,1);
    TE_SetString("setDelays",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    for(int i = 0; i < decl->inputs.size; i++){
      c->Assignment(PushString(temp,"self->in%d",i),"0");
    }
      
    String content = PushASTRepr(c,temp);
    TE_SetString("inStart",content);
  }

  {
    String simulateBusTemplate = R"FOO(
if(SimulateDatabus){
   self->databus_ready_@{i} = 0;
   self->databus_last_@{i} = 0;

   DatabusAccess* access = &databusBuffer[@{i}];

   if(self->databus_valid_@{i}){
      if(access->latencyCounter > 0){
         access->latencyCounter -= 1;
      } else {
         char* ptr = (char*) (self->databus_addr_@{i});

         int transferLength = self->databus_len_@{i};
         if(transferLength < 0){
            transferLength = 0;
         }

         int countersLength = ALIGN_UP(transferLength,sizeOfData) / sizeOfData;

         int trueLength = sizeOfData;
         if(access->counter >= countersLength - 1){
            // Last transfer, need to take into account length to not overwrite data
            int amountTransfered = (access->counter * sizeOfData);
            trueLength = transferLength - amountTransfered;
            if(trueLength < 0){
               versat_printf("TrueLength is negative: %d %d %d %d\n",transferLength,countersLength,amountTransfered,trueLength);
            }
         }

         if(self->databus_wstrb_@{i} == 0){
            if(trueLength > sizeof(self->databus_rdata_@{i})){
              versat_printf("TrueLength greater then size of rdata %d > %d\n",trueLength,sizeof(self->databus_rdata_@{i}));
            }
            if(ptr == nullptr){
              memset(&self->databus_rdata_@{i},0xdf,sizeof(self->databus_rdata_@{i}));
            } else {
              memset(&self->databus_rdata_@{i},0,sizeof(self->databus_rdata_@{i}));
              memcpy(&self->databus_rdata_@{i},&ptr[access->counter * sizeOfData],MIN(sizeof(self->databus_rdata_@{i}),trueLength));
            }
         } else { // self->databus_wstrb_@{i} != 0
            if(trueLength > sizeof(self->databus_wdata_@{i})){
              versat_printf("TrueLength greater then size of wdata %d > %d\n",trueLength,sizeof(self->databus_wdata_@{i}));
            }

            if(ptr != nullptr){
              memcpy(&ptr[access->counter * sizeOfData],&self->databus_wdata_@{i},MIN(sizeof(self->databus_wdata_@{i}),trueLength));
            }
         }
         self->databus_ready_@{i} = 1;

         if(access->counter >= countersLength - 1){
            access->counter = 0;
            self->databus_last_@{i} = 1;
         } else {
            access->counter += 1;
         }

         access->latencyCounter = MEMORY_LATENCY;
      }
   }

   self->eval();
}
                                     )FOO""";

    auto* s = StartString(temp);
    for(int i = 0; i < decl->numberDatabus; i++){
      Hashmap<String,String>* vals = PushHashmap<String,String>(temp,1);
      vals->Insert("i",PushString(temp,"%d",i));
      TE_SimpleSubstitute(s,simulateBusTemplate,vals);
    }
    String content = EndString(temp,s);
      
    TE_SetString("databusSim",content);
  }
    
  {
    CEmitter* c = StartCCode(CCode1,CCode2);
    c->Comment("Extract state from model");
      
    if(decl->states.size > 0){
      c->RawLine("AcceleratorState* state = (AcceleratorState*) &stateBuffer;");
    }

    for(int i = 0; i < decl->states.size; i++){
      Wire w = decl->states[i];
        
      c->Assignment(PushString(temp,"state->%.*s",UN(w.name)),PushString(temp,"self->%.*s",UN(w.name)));
    }
      
    String content = PushASTRepr(c,temp);
    TE_SetString("saveState",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    if(allConfigsVerilatorSide.size){
      c->RawLine(R"FOO(
  AcceleratorConfig* config = (AcceleratorConfig*) &configBuffer;
  //AcceleratorStatic* statics = (AcceleratorStatic*) &staticBuffer;
                     )FOO");

      for(WireExtra wire : allConfigsVerilatorSide){
        // TODO: Need to fix this. Broken after the change to the parameters
#if 0
        if(wire.bitSize != 64){
          String format = S8(R"FOO(
  once{
     if((long long int) @{1}@{0} >= ((long long int) 1 << @{2})){
      printf("[Once] Warning, configuration value contains more bits\n");
      printf("set than the hardware unit is capable of handling\n");
      printf("Name: %s, BitSize: %d, Value: 0x%lx\n","@{0}",@{2},@{1}@{0});
    }
  };
                              )FOO");

          String values[3] = {wire.name,wire.source,PushString(temp,"%d",wire.bitSize)};
            
          c->RawLine(TemplateSubstitute(format,values,temp));
        }
#endif

        if(wire.w.stage == VersatStage_READ){
          c->Assignment(PushString(temp,"self->%.*s",UN(wire.name)),PushString(temp,"%.*s%.*s",UN(wire.source),UN(wire.name)));
        }

        if(wire.w.stage == VersatStage_COMPUTE){
          String format = R"FOO(  self->@{0} = COMPUTED_@{0};
  COMPUTED_@{0} = @{1}@{0};)FOO";
          String values[2] = {wire.name,wire.source};
            
          c->RawLine(TE_Substitute(format,values,temp));
        }
          
        if(wire.w.stage == VersatStage_WRITE){
          String format = R"FOO(  self->@{0} = COMPUTED_@{0};
  COMPUTED_@{0} = WRITE_@{0};
  WRITE_@{0} = @{1}@{0};)FOO";
          String values[2] = {wire.name,wire.source};
            
          c->RawLine(TE_Substitute(format,values,temp));
        }
      }
    }
    
    for(int i = 0; i < decl->numberDatabus; i++){
      c->Assignment(SF("databusBuffer[%d].latencyCounter",i),"INITIAL_MEMORY_LATENCY");
    }
      
    String content = PushASTRepr(c,temp);
    TE_SetString("internalStart",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);
    for(auto wire : allConfigsVerilatorSide){
      if(wire.w.stage == VersatStage_COMPUTE){
        String format = "  COMPUTED_@{0} = 0;";
        String values[2] = {wire.name,wire.source};
            
        c->RawLine(TE_Substitute(format,values,temp));
      }

      if(wire.w.stage == VersatStage_WRITE){
        String format = R"FOO(  COMPUTED_@{0} = 0;
  WRITE_@{0} = 0;)FOO";
        String values[2] = {wire.name,wire.source};
            
        c->RawLine(TE_Substitute(format,values,temp));
      }
    }

#if 0
    c->RawLine(R"FOO(
  AcceleratorConfig* config = (AcceleratorConfig*) &configBuffer;
  AcceleratorStatic* statics = (AcceleratorStatic*) &staticBuffer;
  memset(config,0,sizeof(AcceleratorConfig));)FOO");
#endif
    
    String content = PushASTRepr(c,temp);
    TE_SetString("resetExtraConfigs",content);
  }
  
  {
    CEmitter* c = StartCCode(CCode1,CCode2);
    for(auto wire : allConfigsVerilatorSide){
      if(wire.w.stage == VersatStage_COMPUTE){
        String format = "static iptr COMPUTED_@{0} = 0;";
        String values[2] = {wire.name,wire.source};
            
        c->RawLine(TE_Substitute(format,values,temp));
      }

      if(wire.w.stage == VersatStage_WRITE){
        String format = R"FOO(static iptr COMPUTED_@{0} = 0;
static iptr WRITE_@{0} = 0;)FOO";
        String values[2] = {wire.name,wire.source};
            
        c->RawLine(TE_Substitute(format,values,temp));
      }
    }
    
    String content = PushASTRepr(c,temp);
    TE_SetString("declareExtraConfigs",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    if(info->memoryMappedDecisionBit > 0){
      c->Define("HAS_MEMORY_MAP");
      c->Define("MEMORY_MAP_BITS",SF("%d",info->memoryMappedDecisionBit));
    }
      
    String content = PushASTRepr(c,temp);
    TE_SetString("memoryAccessDefines",content);
  }
  
  if(1){ // nocheckin: HACK - Disabled 
    CEmitter* c = StartCCode(CCode1,CCode2);
    for(int i = 0; i < info->memoryMapped.size; i++){
      int addrStart = info->memoryMapped[i].addrStart;
      int addrSize = info->memoryMapped[i].addrSize;

      String expr = PushString(temp,"(address >= %d && address < %d) ? 1 : 0",addrStart,addrStart + addrSize);
      c->VarDeclare("int",SF("unit_valid_%d",i),expr);

      String expr2 = PushString(temp,"(address >= %d && address < %d)",addrStart,addrStart + addrSize);
      c->If(expr2);
      c->Assignment("validCount","validCount + 1");
      c->Assignment("actualAddress",SF("address - %d",addrStart));
      c->EndIf();
    }

    String content = PushASTRepr(c,temp);
    TE_SetString("memoryUnpack",content);

#if 0
    CEmitter* c = StartCCode(CCode1,CCode2);

    int varIndex = 0;
    for(auto iter = StartIteration(&info); iter.IsValid(); iter = iter.Step()){
      InstanceInfo* unit = iter.CurrentUnit();

      if(unit->isComposite){
        continue;
      }
      if(SYM_IsNil(unit->memMapSym)){
        continue;
      }

      String expr = PushString(temp,"(address >= %d && address <= %d) ? 1 : 0",unit->memStart,unit->memEnd);

      c->VarDeclare("int",SF("unit_valid_%d",varIndex++),expr);

      String expr2 = PushString(temp,"(address >= %d && address <= %d)",unit->memStart,unit->memEnd);
      c->If(expr2);
      c->Assignment("actualAddress",SF("address - %d",unit->memStart));
      c->EndIf();
    }

#endif
  } else {
    TE_SetString("memoryUnpack",{});
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);
    
    for(int i = 0; i < decl->memoryMapped.size; i++){
      c->Assignment(SF("    self->unit_valid_%d",i),SF("unit_valid_%d",i));
    }

    String content = PushASTRepr(c,temp);
    TE_SetString("memorySetValid",content);
  }

  {
    CEmitter* c = StartCCode(CCode1,CCode2);
    
    for(int i = 0; i < decl->memoryMapped.size; i++){
      c->Assignment(SF("    self->unit_valid_%d",i),"0");
    }

    String content = PushASTRepr(c,temp);
    TE_SetString("memoryUnsetValid",content);
  }
  
  TE_SetString("typeName",decl->name);
    
  if(decl->singleInterfaces & DECL_SingleInterface_DONE){
    TE_SetString("done","self->done");
  } else {
    TE_SetString("done","true");
  }

  TE_SetNumber("databusDataSize",globalOptions.databusDataSize);
#endif

  String res = TE_ProcessTemplate(out,META_WrapperTemplate);
  return res;
}

String GEN_OutputFirmware(GEN_TopInfo* info,Arena* out){
  TEMP_REGION(temp,out);
  
  FREE_ARENA(CCode1);
  FREE_ARENA(CCode2);

  CEmitter* c = StartCCode(CCode1,CCode2);

  for(int i = 0; i < info->registers.size; i++){
    VersatRegister reg = info->registers[i];
    String name = VersatRegister_Name(reg);
    String fullName = PushString(temp,"VersatRegister_%.*s",UN(name));
    c->Define(fullName,SF("%d",i));
  }

#if 0
  {
    CEmitter* c = StartCCode(CCode1,CCode2);

    // TODO: BAD
    c->RawLine("AcceleratorConfig* config = (AcceleratorConfig*) accelConfig;\n");
    EmitDisableReadsAndWrites(c,"config",info->info);

    String content = PushASTRepr(c,temp);
    TE_SetString("disableReadsAndWrites",content);
  }
#endif
  
  String content = PushASTRepr(c,temp);
  TE_SetString("registerLocation",content);

  String dmaExists = R"FOO(
  if(enableDMA && (dataInsideVersat != destInsideVersat)){
    if(destInsideVersat){
      destInt = destInt - versat_base;
    }
    if(dataInsideVersat){
      dataInt = dataInt - versat_base;
    }

    MEMSET(versat_base,VersatRegister_DmaInternalAddress,destInt); 
    MEMSET(versat_base,VersatRegister_DmaExternalAddress,dataInt);
    MEMSET(versat_base,VersatRegister_DmaTransferLength,size); // Byte size
    MEMSET(versat_base,VersatRegister_DmaControl,0x1); // Start DMA

    while(1){
      int val = MEMGET(versat_base,VersatRegister_DmaControl);
      if(val) break;
    }
  } else {
    volatile int* destView = (volatile int*) dest;
    volatile int* dataView = (volatile int*) data;
    for(int i = 0; i < size / sizeof(int); i++){
      destView[i] = dataView[i];
    }
  }
)FOO";

  String dmaDoesNotExist = R"FOO(
    volatile int* destView = (volatile int*) dest;
    volatile int* dataView = (volatile int*) data;
    for(int i = 0; i < size / sizeof(int); i++){
      destView[i] = dataView[i];
    }
)FOO";

  if(globalOptions.useDMA){
    TE_SetString("dmaStuff",dmaExists);
  } else {
    TE_SetString("dmaStuff",dmaDoesNotExist);
  }

  String content3 = R"FOO(
// ======================================
// Debug facilities

static inline void DebugEndAccelerator(int upperBound){
  for(int i = 0; i < upperBound; i++){  
    volatile int val = MEMGET(versat_base,VersatRegister_Control);
    if(val){
      return;
    }
  } 
  versat_printf("Accelerator reached upperbound\n");
}

static inline void DebugRunAcceleratorOnce(int upperbound){ // times inside value amount
   versat_printf("Gonna start accelerator: (%x,%d,%d)\n",versat_base, VersatRegister_Control);

   MEMSET(versat_base,VersatRegister_Control,1);
   DebugEndAccelerator(upperbound);

   versat_printf("Ended accelerator\n");
}

void DebugRunAccelerator(int times, int maxCycles){
  // Accelerator was previously stuck
  if(!MEMGET(versat_base, VersatRegister_Control)){
    versat_printf("Versat accel was previously stuck, before current call to DebugRunAccelerator\n");
    return;
  }

  for(int i = 0; i < times; i++){
    DebugRunAcceleratorOnce(maxCycles);
  } 
}

VersatDebugState VersatDebugGetState(){
  VersatDebugState res = {};

  int debugState = MEMGET(versat_base,VersatRegister_Debug); 
  res.databusIsActive = (debugState & 0x1);

  return res;
}

void VersatDebugSetStage(int stage){
   MEMSET(versat_base,VersatRegister_DebugStage,stage);
}
)FOO";

  if(globalOptions.insertDebugRegisters){
    TE_SetString("debugStuff",content3);
  } else {
    TE_SetString("debugStuff",{});
  }

  String content2 = R"FOO(
VersatProfile VersatProfileGet(){
  VersatProfile res = {};              
  
  union {
    uint64_t u64;
    int i32[2];
  } conv;

  conv.i32[0] = MEMGET(versat_base,VersatRegister_ProfileRunCount);
  conv.i32[1] = MEMGET(versat_base,VersatRegister_ProfileRunCount2);
  res.runCount = conv.u64;

  conv.i32[0] = MEMGET(versat_base,VersatRegister_ProfileCyclesSinceLastReset);
  conv.i32[1] = MEMGET(versat_base,VersatRegister_ProfileCyclesSinceLastReset2);
  res.cyclesSinceLastReset = conv.u64;

  conv.i32[0] = MEMGET(versat_base,VersatRegister_ProfileRunningCycles);
  conv.i32[1] = MEMGET(versat_base,VersatRegister_ProfileRunningCycles2);
  res.runningCycles = conv.u64;

  conv.i32[0] = MEMGET(versat_base,VersatRegister_ProfileDatabusValid);
  conv.i32[1] = MEMGET(versat_base,VersatRegister_ProfileDatabusValid2);
  res.databusValid = conv.u64;

  conv.i32[0] = MEMGET(versat_base,VersatRegister_ProfileDatabusValidAndReady);
  conv.i32[1] = MEMGET(versat_base,VersatRegister_ProfileDatabusValidAndReady2);
  res.databusValidAndReady = conv.u64;

  conv.i32[0] = MEMGET(versat_base,VersatRegister_ProfileConfigurationsSet);
  conv.i32[1] = MEMGET(versat_base,VersatRegister_ProfileConfigurationsSet2);
  res.configurationsSet = conv.u64;

  conv.i32[0] = MEMGET(versat_base,VersatRegister_ProfileConfigurationsSetWhileRunning);
  conv.i32[1] = MEMGET(versat_base,VersatRegister_ProfileConfigurationsSetWhileRunning2);
  res.configurationsSetWhileRunning = conv.u64;

  return res;
}

void VersatProfileReset(){
  MEMSET(versat_base,VersatRegister_ProfileControl,1);
}

// 0 to 100
static uint64_t Percentage(uint64_t smaller,uint64_t bigger){
  if(bigger == 0){
    return 0;
  }

  uint64_t percent = ((smaller * 100) / bigger);

  return percent;
}

void VersatPrintProfile(VersatProfile p){
  versat_printf("Runs profiled:%llu\n", p.runCount);
  versat_printf("Total cycles:%llu\n", p.cyclesSinceLastReset);
  versat_printf("  Cycles with accelerator running: %llu (%llu%%)\n", p.runningCycles,
        Percentage(p.runningCycles, p.cyclesSinceLastReset));
  versat_printf("    Cycles databus valid: %llu (%llu%%)\n", p.databusValid,
        Percentage(p.databusValid, p.runningCycles));
  versat_printf("    Cycles databus valid and ready: %llu (%llu%%)\n", p.databusValidAndReady,
        Percentage(p.databusValidAndReady, p.runningCycles));
  versat_printf("      Databus efficiency: %llu%%\n",
        Percentage(p.databusValidAndReady, p.databusValid));

  versat_printf("Configurations set: %llu\n", p.configurationsSet);
  versat_printf("  Configurations set while accel running: %llu\n",
        p.configurationsSetWhileRunning);
  versat_printf("  Configurations efficiency: %llu%%\n",
        Percentage(p.configurationsSet, p.configurationsSetWhileRunning));
}
)FOO";

  if(globalOptions.insertProfilingRegisters){
    TE_SetString("profileStuff",content2);
  } else {
    TE_SetString("profileStuff",{});
  }
  
  String res = TE_ProcessTemplate(out,META_FirmwareTemplate,true);
  return res;
}


String GEN_OutputTestbench(FUDeclaration* decl,Arena* out){
  TEMP_REGION(temp,nullptr);
  NOT_IMPLEMENTED();

#if 0

  VerilogModuleInterface* interface = GenerateModuleInterface(decl,temp);
  Array<VerilogPortSpec> databus = ObtainGroupByName(interface,"Databus");
  Array<VerilogPortSpec> externalMemory = ObtainGroupByName(interface,"ExternalMemory");
  
  bool containsRun = false;
  bool containsRunning = false;
  bool containsDone = false;
  if(decl->singleInterfaces & SingleInterfaces_RUNNING){
    containsRunning = true;
  }
  if(decl->singleInterfaces & SingleInterfaces_RUN){
    containsRun = true;
  }
  if(decl->singleInterfaces & SingleInterfaces_DONE){
    containsDone = true;
  }

  // NOTE: Kinda hacky way of making everything a wire (needed since these connect directly to a module)
  for(VerilogPortSpec& spec : databus){
    spec.dir = WireDir_OUTPUT;
  }
#if 0
  for(VerilogPortSpec& spec : memoryMapped){
    spec.dir = WireDir_OUTPUT;
  }
#endif
  for(VerilogPortSpec& spec : externalMemory){
    spec.dir = WireDir_OUTPUT;
  }
  
  Array<VerilogPortSpec> allPorts = ExtractAllPorts(interface,temp);

  // NOTE: Same as allPorts except we remove some wires from allPorts after this
  //       Basically this is a clean version
  Array<VerilogPortSpec> processedPorts = ExtractAllPorts(interface,temp);
  
  auto FindPortIndexByProperty = [](Array<VerilogPortSpec> ports,SpecialPortProperties prop) -> Opt<int>{
    Opt<int> index = {};
    for(int i = 0; i < ports.size; i++){
      VerilogPortSpec spec = ports[i];
      if(spec.props & prop){
        index = i;
      }
    }

    return index;
  };
  
  Opt<int> clkPortIndex = FindPortIndexByProperty(allPorts,SpecialPortProperties_IsClock);
  String clkName = {};
  if(clkPortIndex.has_value()){
    clkName = allPorts[clkPortIndex.value()].name;
    allPorts = RemoveElement(allPorts,clkPortIndex.value(),temp);
  }

  Opt<int> resetWireIndex = FindPortIndexByProperty(allPorts,SpecialPortProperties_IsReset);
  String resetWire = {};
  if(resetWireIndex.has_value()){
    resetWire = allPorts[resetWireIndex.value()].name; 
  }
  
  VEmitter* m = StartVCode(temp);
  m->Timescale("1ns","1ps");
  m->Module(SF("%.*s_tb",UN(decl->name)));

  for(Parameter p : decl->parameters){
    String repr = SYM_Repr(p.defaultVal,temp);

    // NOTE: Since AXI_ADDR_W is usually used to instantiate a memory,
    // need to severely reduce size otherwise simulation can get stuck
    // and not even a warning is given
    if(CompareString(p.name,"AXI_ADDR_W")){
      repr = "16";
    }

    m->LocalParam(p.name,repr);
  }
  
  m->DeclareInterface(interface,"");
  
  if(Empty(clkName)){
    m->RawStatement("`define ADVANCE #(10)");
  } else {
    m->LocalParam("CLOCK_PERIOD","10");
    m->Blank();
    m->RawStatement(SF("initial %.*s = 0",UN(clkName)));
    m->RawStatement(SF("always #(CLOCK_PERIOD/2) %.*s = ~%.*s",UN(clkName),UN(clkName)));
    m->RawStatement(SF("`define ADVANCE @(posedge %.*s) #(CLOCK_PERIOD/2)",UN(clkName)));
    m->Blank();
  }  

  m->StartInstance(decl->name,"uut");
  for(Parameter p : decl->parameters){
    m->InstanceParam(p.name,p.name);
  }
  m->PortConnectInterface(interface,"");
  m->EndInstance();

  // NOTE: These interfaces are handled by module instantiation.
  bool containsDatabus = RemoveGroupInPlace(interface,"Databus");
  bool containsMemoryMapped = RemoveGroupInPlace(interface,"MemoryMapped");
  bool containsMemories = RemoveGroupInPlace(interface,"ExternalMemory");
  
  auto AdvanceClock = [m](bool blanks = true) -> void{
    if(blanks){
      m->Blank();
    }
    m->RawStatement("`ADVANCE");
    if(blanks){
      m->Blank();
    }
  };

  if(containsMemoryMapped){
    m->Task("Write");

    Opt<VerilogPortSpec> addressSpec = GetPortSpecByName(allPorts,"addr");
    Opt<VerilogPortSpec> dataSpec = GetPortSpecByName(allPorts,"wdata");

    m->Input("addr_i",addressSpec.value().size);
    m->Input("data_i",dataSpec.value().size);

    AdvanceClock();

    m->Set("valid","1");
    m->Set("wstrb","~0");
    m->Set("addr","addr_i");
    m->Set("wdata","data_i");

    AdvanceClock();

    m->Set("valid","0");
    m->Set("wstrb","0");
    m->Set("addr","0");
    m->Set("wdata","0");
    
    m->EndTask();
  }

  if(containsDatabus){
    Opt<VerilogPortSpec> addressSpec = GetPortSpecByName(allPorts,"databus_addr_0");
    Opt<VerilogPortSpec> dataSpec = GetPortSpecByName(allPorts,"databus_wdata_0");
    Opt<VerilogPortSpec> lenSpec = GetPortSpecByName(allPorts,"databus_len_0");

    SYM_Expr dataW = dataSpec.value().size;
    SYM_Expr addrW = addressSpec.value().size;
    SYM_Expr lenW = lenSpec.value().size;
    
    m->Reg("insertValue",1);
    m->Reg("addrToInsert",addrW);
    m->Reg("valueToInsert",dataW);

    m->StartInstance("SimHelper_DatabusMem","DatabusMem");
    m->InstanceParam("DATA_W",dataW);
    m->InstanceParam("ADDR_W",addrW);
    m->InstanceParam("LEN_W",lenW);
    
    m->PortConnect(INT_IOb,"_0");
    m->PortConnect("insertValue","insertValue");
    m->PortConnect("addrToInsert","addrToInsert");
    m->PortConnect("valueToInsert","valueToInsert");

    if(!Empty(clkName)){
      m->PortConnect("clk",clkName);
    }
    if(!Empty(resetWire)){
      m->PortConnect("rst",resetWire);
    }
    
    m->EndInstance();

    m->Task("WriteMemory");
    
    m->Input("addr_i",addrW);
    m->Input("data_i",dataW);

    m->Set("insertValue","1");
    m->Set("addrToInsert","addr_i");
    m->Set("valueToInsert","data_i");

    AdvanceClock(true);

    m->Set("insertValue","0");
    m->Set("addrToInsert","0");
    m->Set("valueToInsert","0");

    AdvanceClock();
    
    m->EndTask();
  }

  if(containsMemories){
    for(int i = 0; i <  decl->externalMemory.size; i++){
      ExternalMemorySymbolic ext  =  decl->externalMemory[i];
      FULL_SWITCH(ext.type){
      case ExternalMemoryType_DP: {
        m->StartInstance(DP_ModuleName,SF("ext_dp_%d",i));

        m->InstanceParam("A_DATA_W",ext.dp[0].dataSizeOut);
        m->InstanceParam("B_DATA_W",ext.dp[1].dataSizeOut);
        m->InstanceParam("ADDR_W",ext.dp[0].bitSize);

        m->PortConnect("dinA_i",SF("ext_dp_out_%d_port_0",i));
        m->PortConnect("addrA_i",SF("ext_dp_addr_%d_port_0",i));
        m->PortConnect("enA_i",SF("ext_dp_enable_%d_port_0",i));
        m->PortConnect("weA_i",SF("ext_dp_write_%d_port_0",i));
        m->PortConnect("doutA_o",SF("ext_dp_in_%d_port_0",i));

        m->PortConnect("dinB_i",SF("ext_dp_out_%d_port_1",i));
        m->PortConnect("addrB_i",SF("ext_dp_addr_%d_port_1",i));
        m->PortConnect("enB_i",SF("ext_dp_enable_%d_port_1",i));
        m->PortConnect("weB_i",SF("ext_dp_write_%d_port_1",i));
        m->PortConnect("doutB_o",SF("ext_dp_in_%d_port_1",i));

        m->PortConnect("clk_i","clk");
        
        m->EndInstance();
      } break;
      case ExternalMemoryType_2P: {
        m->StartInstance(TWO_P_ModuleName,SF("ext_2p_%d",i));
        {
          m->InstanceParam("W_DATA_W",ext.tp.dataSizeOut);
          m->InstanceParam("R_DATA_W",ext.tp.dataSizeIn);
          m->InstanceParam("ADDR_W",ext.tp.bitSizeIn);

          m->PortConnect("w_en_i",SF("ext_2p_write_%d",i));
          m->PortConnect("w_addr_i",SF("ext_2p_addr_out_%d",i));
          m->PortConnect("w_data_i",SF("ext_2p_data_out_%d",i));

          m->PortConnect("r_en_i",SF("ext_2p_read_%d",i));
          m->PortConnect("r_addr_i",SF("ext_2p_addr_in_%d",i));
          m->PortConnect("r_data_o",SF("ext_2p_data_in_%d",i));

          m->PortConnect("clk_i","clk");
        }
        m->EndInstance();
      } break;
    }
    }
  }
  
  if(containsRun){
    if(!containsDone){
      m->LocalParam("SIM_TIME_NO_DONE","10");
      m->Integer("i");
    }
   
    m->Task("RunAccelerator");

    m->SetForced("run","1",true);

    AdvanceClock();

    m->SetForced("run","0",true);
    if(containsRunning){
      m->SetForced("running","1",true);
    }

    AdvanceClock();
    
    if(containsDone){
      m->Loop("","~done","");
      AdvanceClock(false);
      m->EndLoop();
    } else {
      m->Loop("i = 0","i < SIM_TIME_NO_DONE","i = i + 1");
      AdvanceClock(false);
      m->EndLoop();
    }

    if(containsRunning){
      m->SetForced("running","0",true);
    }
  
    m->EndTask();
  } else if(containsRunning){
    if(!containsDone){
      m->LocalParam("SIM_TIME_NO_DONE","10");
      m->Integer("i");
    }

    m->Task("SetRunning");
    m->SetForced("running","1",true);
    
    AdvanceClock();

    if(containsDone){
      m->Loop("","~done","");
      AdvanceClock();
      m->EndLoop();
    } else {
      m->Loop("i = 0","i < SIM_TIME_NO_DONE","i = i + 1");
      AdvanceClock(false);
      m->EndLoop();
    }

    m->SetForced("running","0",true);

    m->EndTask();
  }
  
  m->InitialBlock();

  m->RawStatement("`ifdef VCD");
  m->RawStatement("$dumpfile(\"uut.vcd\")");
  m->RawStatement("$dumpvars()");
  m->RawStatement("`endif // VCD");
  
  for(VerilogPortSpec port : processedPorts){
    if(port.dir == WireDir_INPUT){
      m->Set(port.name,"0");
    }
  }
  if(containsDatabus){
    m->Set("insertValue",0);
    m->Set("addrToInsert",0);
    m->Set("valueToInsert",0);
  }
  
  if(Empty(resetWire)){
    AdvanceClock();
  } else {
    AdvanceClock();
    m->Set(resetWire,"1");
    AdvanceClock();
    m->Set(resetWire,"0");
    AdvanceClock();
  }
  m->Comment("Insert logic after this point");
  m->Blank();
  m->Blank();
  m->Blank();
  m->RawStatement("$finish()");

  m->EndBlock();
  
  m->EndModule();

  VAST* ast = EndVCode(m);
  auto* builder = StartString(temp);
  Repr(ast,builder);
  String content = EndString(out,builder);


  return content;
#endif
  
  NOT_IMPLEMENTED();
  return {};
}


















#if 1
// ======================================
// Gen Type

#include "configurations.hpp"

GEN_StructInfo* GEN_GenerateConfigStruct(COM_Unit* top,GEN_StructInfoList* allStructs,Arena* out){
  GEN_StructElem* head = 0;
  GEN_StructElem* tail = 0;

  FUDeclaration* decl = top->decl;
  
  if(!DECL_IsComposite(top->decl)){
    GEN_StructInfo* info = 0;
    LL_Find(allStructs->head,next,info,it->name == decl->name);

    if(!info){
      for(Wire w : decl->configs){
        GEN_StructElem* elem = PushStruct<GEN_StructElem>(out);
        elem->simpleTypename = "iptr";
        elem->name = w.name;
        LL_Append(head,tail,next,elem);
      }

      info = PushStruct<GEN_StructInfo>(out);
      info->elements = head;
      info->name = PushString(out,"%.*s",UN(decl->name));

      LL_Append(allStructs->head,allStructs->tail,next,info);
    }

    return info;
  }

  for(COM_Unit* ptr = top->decl->fixedDelays.units.head; ptr; ptr = ptr->next){
    if(ptr->decl->configs.size == 0){
      continue;
    }

    if(false /* ptr->isMerge */){
#if 0
      GEN_StructElem* mergeHead = 0;
      GEN_StructElem* mergeTail = 0;

      COM_Unit* newTop = top->decl->fixedDelays.units.head;

      int partIndex = 0;
      for(COM_Unit* newPtr = newTop; newPtr; partIndex++,newPtr = newPtr->mergeNext){
        MergePartition part = ptr->decl->info.infos[partIndex];
        
        String partName = part.name;

        GEN_StructInfo* info = GEN_GenerateConfigStruct(partName,newPtr,out);

        GEN_StructElem* elem = PushStruct<GEN_StructElem>(out);
        elem->name = PushString(out,partName);
        elem->type = info;

        LL_Append(mergeHead,mergeTail,next,elem);
      }

      GEN_StructElem* elem = PushStruct<GEN_StructElem>(out);
      elem->childs = mergeHead;

      LL_Append(head,tail,next,elem);
#endif
    } else {
      GEN_StructElem* elem = PushStruct<GEN_StructElem>(out);
      elem->type = GEN_GenerateConfigStruct(ptr,allStructs,out);
      elem->name = ptr->name;
      LL_Append(head,tail,next,elem);
    }
  }

  GEN_StructInfo* res = PushStruct<GEN_StructInfo>(out);
  res->elements = head;
  res->name = PushString(out,"%.*s",UN(top->name));

  LL_Append(allStructs->head,allStructs->tail,next,res);

  return res;
}

GEN_StructInfo* GEN_GenerateStateStruct(COM_Unit* top,GEN_StructInfoList* allStructs,Arena* out){
  GEN_StructElem* head = 0;
  GEN_StructElem* tail = 0;

  FUDeclaration* decl = top->decl;
  
  if(!DECL_IsComposite(top->decl)){
    GEN_StructInfo* info = 0;
    LL_Find(allStructs->head,next,info,it->name == decl->name);

    if(!info){
      for(Wire w : decl->states){
        GEN_StructElem* elem = PushStruct<GEN_StructElem>(out);
        elem->simpleTypename = "iptr";
        elem->name = w.name;
        LL_Append(head,tail,next,elem);
      }

      info = PushStruct<GEN_StructInfo>(out);
      info->elements = head;
      info->name = PushString(out,"%.*s",UN(decl->name));

      LL_Append(allStructs->head,allStructs->tail,next,info);
    }

    return info;
  }

  for(COM_Unit* ptr = top->decl->fixedDelays.units.head; ptr; ptr = ptr->next){
    if(ptr->decl->states.size == 0){
      continue;
    }

    if(false /* ptr->isMerge */){
#if 0
      GEN_StructElem* mergeHead = 0;
      GEN_StructElem* mergeTail = 0;

      COM_Unit* newTop = top->decl->fixedDelays.units.head;

      int partIndex = 0;
      for(COM_Unit* newPtr = newTop; newPtr; partIndex++,newPtr = newPtr->mergeNext){
        MergePartition part = ptr->decl->info.infos[partIndex];
        
        String partName = part.name;

        GEN_StructInfo* info = GEN_GenerateConfigStruct(partName,newPtr,out);

        GEN_StructElem* elem = PushStruct<GEN_StructElem>(out);
        elem->name = PushString(out,partName);
        elem->type = info;

        LL_Append(mergeHead,mergeTail,next,elem);
      }

      GEN_StructElem* elem = PushStruct<GEN_StructElem>(out);
      elem->childs = mergeHead;

      LL_Append(head,tail,next,elem);
#endif
    } else {
      GEN_StructElem* elem = PushStruct<GEN_StructElem>(out);
      elem->type = GEN_GenerateStateStruct(ptr,allStructs,out);
      elem->name = ptr->name;
      LL_Append(head,tail,next,elem);
    }
  }

  GEN_StructInfo* res = PushStruct<GEN_StructInfo>(out);
  res->elements = head;
  res->name = PushString(out,"%.*s",UN(top->name));

  LL_Append(allStructs->head,allStructs->tail,next,res);

  return res;
}

bool GEN_IsSimpleType(GEN_StructElem* elem){
  bool res = !Empty(elem->simpleTypename);

  if(res){
    Assert(elem->type == 0);
  }
  return res;
}

bool GEN_IsUnion(GEN_StructElem* top){
  bool res = (top->childs && top->childs->next); // Union needs at least two childs
  return res;
}

String GEN_TypeName(GEN_StructElem* elem){
  String res = {};
  if(GEN_IsSimpleType(elem)){
    res = elem->simpleTypename;
  } else {
    res = elem->type->name;
  }
  return res;
}

String GEN_Repr(GEN_StructInfo* info,Arena* out){
  TEMP_REGION(temp,out);

  auto b = StartString(temp);

  b->PushString("typedef struct{\n");

  for(GEN_StructElem* ptr = info->elements; ptr; ptr = ptr->next){
    if(GEN_IsUnion(ptr)){
      b->PushString("  union {\n");
    }

    if(ptr->childs){
      for(GEN_StructElem* child = ptr->childs; child; child = child->next){
        String typeName = GEN_TypeName(child);
        
        b->PushString("    %.*s %.*s;\n",UN(typeName),UN(child->name));
      }
    } else {
      String typeName = GEN_TypeName(ptr);

      b->PushString("  %.*s %.*s;\n",UN(typeName),UN(ptr->name));
    }

    if(GEN_IsUnion(ptr)){
      b->PushString("  };\n");
    }
  }

  b->PushString("} %.*s;\n",UN(info->name));

  String res = EndString(out,b);
  return res;
}

#endif

#include "globals.hpp"

GEN_TopInfo GEN_CalculateTopInfo(FUDeclaration* topLevel,Arena* out){
  TEMP_REGION(temp,out);

  COM_Unit* units = topLevel->baseGraph.units.head;

  bool error = 0;

  int databusCount = 0;
  int staticCount = 0; // TODO

  SYM_Expr configBitCount = SYM_0;
  SYM_Expr stateBitCount = SYM_0;

  // Calculate and Insert Versat specific registers =============================
  auto registerList = PushList<VersatRegister>(temp);

  auto AddRegister = [&](VersatRegister reg){
    // Why did I care about bitcount for registers?
    //configBitCount = configBitCount + SYM_DataW;
    //stateBitCount = stateBitCount + SYM_DataW;
    *registerList->PushElem() = reg;
  };

  AddRegister(VersatRegister_Control);
  
  if(globalOptions.useDMA){
    AddRegister(VersatRegister_DmaInternalAddress);
    AddRegister(VersatRegister_DmaExternalAddress);
    AddRegister(VersatRegister_DmaTransferLength);
    AddRegister(VersatRegister_DmaControl);
    
    databusCount += 1; // For the DMA
  }

  if(globalOptions.insertDebugRegisters){
    AddRegister(VersatRegister_Debug);
    AddRegister(VersatRegister_DebugStage);
  }

  if(globalOptions.insertProfilingRegisters){
    for(VersatRegister reg : ProfilingVersatRegisters){
      AddRegister(reg);
    }
  }
  
  if(globalOptions.insertCaptureDatabusRegisters){
    AddRegister(VersatRegister_CaptureDatabusIndexAndReadWrite);
    AddRegister(VersatRegister_CaptureDatabusCount);
    AddRegister(VersatRegister_CaptureDatabusGetAddr);
    AddRegister(VersatRegister_CaptureDatabusGetValue);
    AddRegister(VersatRegister_CaptureDatabusGetDidCapture);
  }

  // Finish versat registers ====================================================
  Array<VersatRegister> versatRegisters = PushArray(out,registerList);

  // Calculate final wire info ==================================================
  auto configBuild = PushList<GEN_Wire>(temp);
  auto stateBuild = PushList<GEN_Wire>(temp);

  SYM_Expr configAccum = SYM_0;
  SYM_Expr stateAccum = SYM_0;
  int configIndex = 0;
  int stateIndex = 0;
  int staticIndex = 0;

  // NOTE: The 'index * 4' is because address is byte aligned

  for(VersatRegister v : versatRegisters){
    SYM_Expr size = SYM_DataW;

    GEN_Wire* c = configBuild->PushElem();
    GEN_Wire* s = stateBuild->PushElem();

    c->name = s->name = VersatRegister_Name(v);
    c->size = s->size = size;
    c->startBit = s->startBit = configAccum;
    c->address = s->address = configIndex * 4;
    c->type |= GEN_WireType_IS_VERSAT;
    s->type |= GEN_WireType_IS_VERSAT;

    //configAccum = configAccum + size;
    //stateAccum = stateAccum + size;
    configIndex += 1;
    stateIndex += 1;
  }

  
  for(Wire w : topLevel->configs){
    GEN_Wire* c = configBuild->PushElem();

    c->name = w.name;
    c->stage = w.stage;
    c->size = w.sizeExpr;
    c->startBit = configAccum;
    c->address = configIndex * 4;

    configBitCount = configBitCount + w.sizeExpr;

    configAccum = configAccum + w.sizeExpr;
    configIndex += 1;
  }
  for(Wire w : topLevel->states){
    GEN_Wire* c = stateBuild->PushElem();

    c->name = w.name;
    c->stage = w.stage;
    c->size = w.sizeExpr;
    c->startBit = stateAccum;
    c->address = stateIndex * 4;

    stateBitCount = stateBitCount + w.sizeExpr;

    stateAccum = stateAccum + w.sizeExpr;
    stateIndex += 1;
  }

  SYM_Expr staticAccum = configAccum;
  for(DECL_StaticWire w : topLevel->statics){
    GEN_Wire* c = configBuild->PushElem();

    c->name = w.wire.name;
    c->stage = w.wire.stage;
    c->size = w.wire.sizeExpr;
    c->startBit = staticAccum;
    c->address = (configIndex + staticIndex) * 4;
    c->type |= GEN_WireType_IS_STATIC;

    staticAccum = staticAccum + w.wire.sizeExpr;
    staticIndex += 1;
  }

  SYM_Expr delayAccum = staticAccum;
  for(int i = 0; i < topLevel->numberInputDelays; i++){
    GEN_Wire* c = configBuild->PushElem();

    c->name = PushString(out,"delay_in%d",i);
    c->stage = VersatStage_COMPUTE;
    c->size = SYM_DelayW;
    c->startBit = delayAccum;
    c->address = (configIndex + staticIndex + i) * 4;
    c->type |= GEN_WireType_IS_DELAY;
    
    delayAccum = delayAccum + SYM_DelayW;
  }
  for(int i = 0; i < topLevel->numberOutputDelays; i++){
    GEN_Wire* c = configBuild->PushElem();

    c->name = PushString(out,"delay_out%d",i);
    c->stage = VersatStage_COMPUTE;
    c->size = SYM_DelayW;
    c->startBit = delayAccum;
    c->address = (configIndex + staticIndex + i) * 4;
    c->type |= GEN_WireType_IS_DELAY;
    
    delayAccum = delayAccum + SYM_DelayW;
  }

  Array<GEN_Wire> configWires = PushArray(out,configBuild);
  Array<GEN_Wire> stateWires = PushArray(out,stateBuild);
  
  int stateCount = stateIndex;
  int totalConfigCount = configIndex + staticIndex + topLevel->numberInputDelays + topLevel->numberOutputDelays;
  databusCount += topLevel->numberDatabus;
  
  // Huffman allocation of memory regions for mem mapped units ==================
  int memoryMappedBit = 0;
  Array<GEN_MMInfo> memoryMapped = {};
  {
    struct HuffmanNode{
      HuffmanNode* next;
      HuffmanNode* prev;
      HuffmanNode* parent;
      HuffmanNode* left;
      HuffmanNode* right;

      String addrName;
      int addrIndex;
      int bitCount;
      bool isBitSet;
    };
 
    int size = topLevel->memoryMapped.size;
    HuffmanNode* nodesHead = 0;
    HuffmanNode* nodesTail = 0;
    for(int i = 0; i < size; i++){
      String addrName = topLevel->memoryMapped[i].addrName;
      HW_Instance in = topLevel->memoryMapped[i].inst;
      SYM_Expr addrSize = HW_GetAddrSize(in,0);
      SYM_EvaluateResult eval = SYM_ConstantEvaluate(addrSize);

      if(eval.Error()){
        ENTER_DEBUG();
        // TODO: Better error message
        printf("Versat cannot handle memory mapped units with parameters for addresses, cannot allocate addresses\n");
        // TODO: Temporarely disabled
        //error = 1;
      }

      HuffmanNode* node = PushStruct<HuffmanNode>(temp);
      node->bitCount = eval.result;
      node->addrIndex = i;
      node->addrName = addrName;
      DLL_Append(nodesHead,nodesTail,next,prev,node);
    }

    if(!error && size > 0){
      HuffmanNode* allHead = 0;
      HuffmanNode* allTail = 0;

      if(nodesHead && !nodesHead->next){
        LL_Append(allHead,allTail,next,nodesHead);
      }

      while(nodesHead && nodesHead->next){
        HuffmanNode* biggestMin = 0;
        HuffmanNode* smallestMin = 0;
        int biggestMinVal = INT_MAX;
        int smallestMinVal = INT_MAX;

        // Find the two smallest ======================================================
        for(HuffmanNode* ptr = nodesHead; ptr; ptr = ptr->next){
          if(ptr->bitCount < smallestMinVal){
            biggestMinVal = smallestMinVal;
            biggestMin = smallestMin;

            smallestMin = ptr;
            smallestMinVal = ptr->bitCount;
          } else if(ptr->bitCount < biggestMinVal){
            biggestMin = ptr;
            biggestMinVal = ptr->bitCount;
          }
        }

        // Move from current list to all list =========================================
        DLL_Remove(nodesHead,nodesTail,next,prev,biggestMin);
        DLL_Remove(nodesHead,nodesTail,next,prev,smallestMin);

        if(smallestMin->addrIndex != -1){
          LL_Append(allHead,allTail,next,smallestMin);
        }
        if(biggestMin->addrIndex != -1){
          LL_Append(allHead,allTail,next,biggestMin);
        }

        HuffmanNode* join = PushStruct<HuffmanNode>(temp);
        join->addrIndex = -1;
        join->bitCount = MAX(biggestMin->bitCount,smallestMin->bitCount) + 1;
        join->left = smallestMin;
        join->left->parent = join;
        join->right = biggestMin;
        join->right->parent = join;
        join->right->isBitSet = 1;
        DLL_Append(nodesHead,nodesTail,next,prev,join);
      }

      HuffmanNode* top = nodesHead;
      memoryMappedBit = top->bitCount;
      memoryMapped = PushArray<GEN_MMInfo>(out,size);

      for(HuffmanNode* ptr = allHead; ptr; ptr = ptr->next){
        Assert(ptr->addrIndex >= 0);

        int addrIndex = ptr->addrIndex;
        int offset = ptr->bitCount;
        int maskSize = memoryMappedBit - offset;

        char* maskBuffer = (char*) PushBytes(out,maskSize);
        for(int i = 0; i < maskSize; i++){
          maskBuffer[i] = '0';
        }

        int addrStart = 0;
        for(HuffmanNode* parent = ptr; parent; parent = parent->parent){
          int index = parent->bitCount - offset;
          if(parent->isBitSet){
            addrStart |= (1 << parent->bitCount);
            maskBuffer[index] = '1';
          }
        }
        
        memoryMapped[addrIndex].decisionMask = String(maskBuffer,maskSize);
        memoryMapped[addrIndex].addrStart = addrStart;
        memoryMapped[addrIndex].addrName = ptr->addrName;
        memoryMapped[addrIndex].addrSize = (1 << ptr->bitCount);
      }
    }
  }

  // TODO: + 2 since we are dealing with byte addresses
  int configMemoryDecisionBit = log2i(totalConfigCount) + 2;
  int stateMemoryDecisionBit = log2i(stateCount) + 2;

  int memoryConfigDecisionBit = MAX(configMemoryDecisionBit,stateMemoryDecisionBit);

  // NOTE: We divide the address space into two regions, config + state vs memory mapped.
  int addressBits = MAX(memoryConfigDecisionBit,memoryMappedBit) + 1;

  // Pack =======================================================================
  GEN_TopInfo res = {};
  res.decl = topLevel;
  res.registers = versatRegisters;
  res.memoryMapped = memoryMapped;
  res.allConfigs = configWires;
  res.allStates = stateWires;

  res.configStart = versatRegisters.size;
  res.staticStart = res.configStart + topLevel->configs.size;
  res.delayStart  = res.staticStart + staticCount;

  res.totalConfigs = topLevel->configs.size + staticCount + topLevel->numberInputDelays + topLevel->numberOutputDelays;
  res.configOnlyBitCount = configBitCount;
  res.configTotalBitCount = delayAccum;
  res.stateCount = stateCount;
  res.stateBitCount = stateBitCount;
  res.addressBits = addressBits;
  res.memoryMappedDecisionBit = memoryMappedBit;
  res.memoryConfigDecisionBit = memoryConfigDecisionBit;
  res.numberDatabus = databusCount;
  
  return res;
}

int GEN_GetRegisterIndex(GEN_TopInfo* info,VersatRegister reg){
  for(int i = 0; i < info->registers.size; i++){
    if(info->registers[i] == reg){
      return i;
    }
  }

  Assert(false && "Should never fail, check control flow that calls this.");
  return 0;
}
