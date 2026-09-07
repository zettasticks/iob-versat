#include "compiler.hpp"

#if 0

COM_Instance* SP_InstantiateModule(SP_Node* moduleDef,Array<ParamNameAndValue> topLevelParams,Arena* out){
  Assert(moduleDef->type = SP_Type_MODULE_DECL);
  
  TEMP_REGION(temp,out);

  //Accelerator* circuit = CreateAccelerator(def.name.identifier,AcceleratorPurpose_MODULE);

  SP_Node* inputs = moduleDef->childs;
  SP_Node* decls = inputs->next;
  SP_Node* funcs = decls->next;

  FREE_ARENA(envArena);
  FREE_ARENA(envArena2);
  COM_Env* env = {};
  env->errorArena = envArena;

  

  //env->circuit = circuit;
  
  return {};

#if 0
  // Pass to check if out instance is used anywhere
  bool addOutputInstance = false;
  for(ConnectionDef* decl : def.connections){
    for(Var v : decl->input.vars){
      if(v.name.identifier == "out"){
        addOutputInstance = true;
        break;
      }
    }

    for(Var v : decl->output.vars){
      if(v.name.identifier == "out"){
        addOutputInstance = true;
        break;
      }
    }
  }
  if(addOutputInstance){
    FUInstance* outInst = CreateOrGetOutput(circuit);

    Entity outEnt = MakeEntity(outInst);
    env->PushEntity("out",outEnt);
    env->table->Insert("out",outInst);
  }

  auto paramList = PushList<ParameterDef>(temp);
  for(ParameterDeclaration param : def.params){
    int val = 0;
   
    bool topOverride = false;
    for(ParamNameAndValue p : topLevelParams){
      if(p.name == param.name.identifier){
        val = p.value;
        topOverride = true;
      }
    }

    if(!topOverride){
      val = env->CalculateConstantExpression(param.defaultValue);
    }

    env->AddParam(param.name,val);

    ParameterDef* def = paramList->PushElem();
    
    def->name = param.name.identifier;
    def->defaultValue = env->SymbolicFromMathExpression(param.defaultValue);
  }
  auto params = PushArray(temp,paramList);

  for(VarDeclaration& decl : def.inputs){
    env->AddInput(decl);
  }

  int shareIndex = 0;
  for(InstanceDeclaration& decl : def.declarations){
    if(decl.modifier == InstanceDeclarationType_SHARE_CONFIG){
      decl.shareIndex = shareIndex++;
    }
  }

  for(InstanceDeclaration& decl : def.declarations){
    for(VarDeclaration& var : decl.declarations){
      env->AddInstance(decl,var);
    }
  }

  auto Recurse = [env](auto Recurse,ConnectionDef* top) -> void{
    FULL_SWITCH(top->type){
    case ConnectionType_NONE: Assert(false);
    case ConnectionType_EQUALITY:{
      env->AddEquality(*top);
    } break;
    case ConnectionType_CONNECTION:{
      env->AddConnection(*top);
    } break;
    case ConnectionType_LOOP:{
      int start = env->CalculateConstantExpression(top->loopStart);
      int end = env->CalculateConstantExpression(top->loopEnd);
      
      for(int i = start; i < end; i++){
        env->PushScope(EnvScopeType_FOR_LOOP);
        env->AddParam(top->loopVar,i);

        for(ConnectionDef* child : top->loopExpressions){
          Recurse(Recurse,child);
        }

        env->PopScope();
      }
    } break;
  }

  };

  for(ConnectionDef* decl : def.connections){
    Recurse(Recurse,decl);
  }

  FUDeclaration* res = RegisterSubUnit(circuit,params,SubUnitOptions_BAREBONES);
  
  {
    TEMP_REGION(temp,nullptr);
    auto list = PushList<ConfigFunction*>(temp);
    for(auto funcDecl : def.configs){
      *list->PushElem() = InstantiateConfigFunction(env,&funcDecl,res,content,globalPermanent);
    };
    
    if(res->info.infos.size){
      res->info.infos[0].userFunctions = PushArray(out,list);
    }
  }

  for(String error : env->errors){
    printf("%.*s\n",UN(error));
  }

  // TODO: We probably want to keep going and only print errors and exit at the top level after processing all the
  //       modules that we have.
  if(!Empty(env->errors)){
    printf("[Error] On environment\n");
    //exit(0);
  }
#endif  
}

#endif
