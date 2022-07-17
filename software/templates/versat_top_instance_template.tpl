#{include "versat_common.tpl"}
`timescale 1ns / 1ps
`include "axi.vh"
//`include "xversat.vh"
//`include "xdefs.vh"
`include "versat_defs.vh"

module versat_instance #(
      parameter ADDR_W = `ADDR_W,
      parameter DATA_W = `DATA_W,
      parameter AXI_ADDR_W = 32
   )
   (
   // Databus master interface
`ifdef VERSAT_IO
   input [`nIO-1:0]                m_databus_ready,
   output [`nIO-1:0]               m_databus_valid,
   output [`nIO*AXI_ADDR_W-1:0]    m_databus_addr,
   input [`nIO*`DATAPATH_W-1:0]    m_databus_rdata,
   output [`nIO*`DATAPATH_W-1:0]   m_databus_wdata,
   output [`nIO*`DATAPATH_W/8-1:0] m_databus_wstrb,
`endif
   // data/control interface
   input                           valid,
   input [ADDR_W-1:0]              addr,
   input [DATA_W/8-1:0]            wstrb,
   input [DATA_W-1:0]              wdata,
   output                          ready,
   output reg [DATA_W-1:0]         rdata,

   input                           clk,
   input                           rst
   );

wire wor_ready;

wire done;
reg run;
reg [31:0] stateRead;
#{if unitsMapped}
wire [31:0] unitRdataFinal;
#{end}

wire we = (|wstrb);
wire memoryMappedAddr = addr[@{memoryConfigDecisionBit}];

// Versat registers and memory access
reg versat_ready;
reg [31:0] versat_rdata;

reg soft_reset;

wire rst_int = (rst | soft_reset);

// Interface does not use soft_rest
always @(posedge clk,posedge rst) // Care, rst because writing to soft reset register
   if(rst) begin
      versat_rdata <= 32'h0;
      versat_ready <= 1'b0;
      soft_reset <= 0;
   end else begin
      versat_ready <= 1'b0;

      if(valid) begin 
         // Config/State register access
         if(!memoryMappedAddr) begin
            versat_ready <= 1'b1;
            versat_rdata <= stateRead;
         end

         // Versat specific registers
         if(addr == 0) begin
            versat_ready <= 1'b1;
            if(we)
               soft_reset <= wdata[1];
            else
               versat_rdata <= {31'h0,done}; 
         end
      end
   end

always @(posedge clk,posedge rst_int)
begin
   if(rst_int) begin
      run <= 1'b0;
   end else begin
      run <= 1'b0;

      if(valid && we && addr == 0)
         run <= wdata[0];
   end
end

assign rdata = (versat_ready ? versat_rdata : unitRdataFinal);

assign ready = versat_ready | wor_ready;

reg [@{versatValues.configurationBits - 1}:0] configdata;
wire [@{versatValues.stateBits - 1}:0] statedata;

wire [@{numberUnits - 1}:0] unitDone;
reg [@{unitsMapped - 1}:0] memoryMappedEnable;
wire[@{unitsMapped - 1}:0] unitReady;

assign wor_ready = (|unitReady);
assign done = (&unitDone && !run);

#{if versatValues.numberConnections}
wire [31:0] #{join ", " for inst instances}
   #{if inst.tempData.outputPortsUsed} 
      #{join ", " for j inst.tempData.outputPortsUsed} output_@{inst.id}_@{j} #{end}
   #{else}
      unused_@{inst.id} #{end}
#{end};
#{end}

#{if unitsMapped}
wire [31:0] unitRData[@{unitsMapped - 1}:0];
assign unitRdataFinal = (#{join "|" for i unitsMapped} unitRData[@{i}] #{end});
#{end}

// Memory mapped
always @*
begin
   memoryMappedEnable = {@{unitsMapped}{1'b0}};
#{if unitsMapped}
   if(valid & memoryMappedAddr)
   begin
   #{set counter 0}
   #{for inst instances}
   #{if inst.declaration.isMemoryMapped}
      #{if inst.versatData.memoryMaskSize}
         if(addr[@{memoryAddressBits - 1}:@{memoryAddressBits - inst.versatData.memoryMaskSize}] == @{inst.versatData.memoryMaskSize}'b@{inst.versatData.memoryMask})
            memoryMappedEnable[@{counter}] = 1'b1;
      #{else}
         memoryMappedEnable[0] = 1'b1;
      #{end}
   #{inc counter}
   #{end}
   #{end}
   end
#{end}
end

// Config writing
always @(posedge clk,posedge rst_int)
begin
   if(rst_int) begin
      configdata <= {@{configurationBits}{1'b0}};
   end else if(valid & we & !memoryMappedAddr) begin
      #{set counter 0}
      #{set addr 1}
      #{for inst instances}
      #{set decl inst.declaration}
      #{for i decl.nConfigs}
      #{set wire decl.configWires[i]}
      if(addr[@{versatValues.configurationAddressBits - 1}:0] == @{addr}) // @{versatBase + addr * 4 |> Hex}
         configdata[@{counter}+:@{wire.bitsize}] <= wdata[@{wire.bitsize - 1}:0]; // @{wire.name} - @{decl.name.str}
      #{inc addr}
      #{set counter counter + wire.bitsize}
      #{end}
      #{end}

      #{for unit accel.staticInfo}
      #{for i unit.nConfigs}
      #{set wire unit.wires[i]}
      if(addr[@{versatValues.configurationAddressBits - 1}:0] == @{addr}) // @{versatBase + addr * 4 |> Hex}
         configdata[@{counter}+:@{wire.bitsize}] <= wdata[@{wire.bitsize - 1}:0]; //  @{unit.module.name.str}_@{unit.name}_@{wire.name}
      #{inc addr}
      #{set counter counter + wire.bitsize}
      #{end}
      #{end}

      #{for inst instances}
      #{set decl inst.declaration}
      #{for i decl.nDelays}
      if(addr[@{versatValues.configurationAddressBits - 1}:0] == @{addr}) // @{versatBase + addr * 4 |> Hex}
         configdata[@{counter}+:32] <= wdata[31:0]; // Delay
      #{inc addr}
      #{set counter counter + 32}
      #{end}
      #{end}
   end
end

// State reading
always @*
begin
   stateRead = 32'h0;
   if(valid & !memoryMappedAddr) begin
      #{set counter 0}
      #{set addr 0}
      #{for inst instances}
      #{set decl inst.declaration}
      #{for i decl.nStates}
      #{set wire decl.stateWires[i]}
      if(addr[@{versatValues.stateAddressBits - 1}:0] == @{addr + 1}) // @{versatBase + addr * 4 |> Hex}
         stateRead = statedata[@{counter}+:@{wire.bitsize}];
      #{inc addr}
      #{set counter counter + wire.bitsize}
      #{end}
      #{end}
   end
end

#{set counter 0}
#{set configDataIndex 0}
#{set stateDataIndex 0}
#{set ioIndex 0}
#{set memoryMappedIndex 0}
#{set delaySeen 0}
#{for inst instances}
#{set decl inst.declaration}
   @{decl.name.str} @{decl.name.str}_@{counter} (
      #{for i inst.tempData.outputPortsUsed}
         .out@{i}(output_@{inst.id}_@{i}),
      #{end} 

      #{for i inst.tempData.inputPortsUsed}
         .in@{i}(output_@{inst.tempData.inputs[i].inst.inst.id}_@{inst.tempData.inputs[i].inst.port}),
      #{end}

      #{for i decl.nConfigs}
      #{set wire decl.configWires[i]}
      #{if decl.type}
         .@{wire.name}(configdata[@{configDataIndex}+:@{wire.bitsize}]),
      #{else}
         .@{wire.name}(configdata[@{configDataIndex}+:@{wire.bitsize}]),
      #{end}
      #{set configDataIndex configDataIndex + wire.bitsize}
      #{end}

      #{for unit decl.staticUnits}
      #{for i unit.nConfigs}
      #{set wire unit.wires[i]}
         .@{unit.module.name.str}_@{unit.name}_@{wire.name}(configdata[@{configDataIndex}+:@{wire.bitsize}]),
      #{set configDataIndex configDataIndex + wire.bitsize}
      #{end}
      #{end}

      #{for i decl.nDelays}
         .delay@{i}(configdata[@{configDataIndex}+:32]),
      #{set configDataIndex configDataIndex + 32}
      #{end}

      #{for i decl.nStates}
      #{set wire decl.stateWires[i]}
      #{if decl.type}
         .@{wire.name}(statedata[@{stateDataIndex}+:@{wire.bitsize}]),
      #{else}
         .@{wire.name}(statedata[@{stateDataIndex}+:@{wire.bitsize}]),
      #{end}
      #{set stateDataIndex stateDataIndex + wire.bitsize}
      #{end}      

      #{if decl.isMemoryMapped}
      .valid(memoryMappedEnable[@{memoryMappedIndex}]),
      .wstrb(wstrb),
      #{if decl.memoryMapBits}
      .addr(addr[@{decl.memoryMapBits - 1}:0]),
      #{else}
      .addr(1'b0), // Shouldnt need but otherwise verilator would complain
      #{end}
      .rdata(unitRData[@{memoryMappedIndex}]),
      .ready(unitReady[@{memoryMappedIndex}]),
      .wdata(wdata),
      #{inc memoryMappedIndex}
      #{end}

      #{if decl.nIOs}
      .databus_ready(m_databus_ready[@{ioIndex} +: @{decl.nIOs}]),
      .databus_valid(m_databus_valid[@{ioIndex} +: @{decl.nIOs}]),
      .databus_addr(m_databus_addr[@{ioIndex * 32} +: @{32 * decl.nIOs}]),
      .databus_rdata(m_databus_rdata[@{ioIndex * 32} +: @{32 * decl.nIOs}]),
      .databus_wdata(m_databus_wdata[@{ioIndex * 32} +: @{32 * decl.nIOs}]),
      .databus_wstrb(m_databus_wstrb[@{ioIndex * 4} +: @{4 * decl.nIOs}]),
      #{set ioIndex ioIndex + decl.nIOs}
      #{end} 
      
      .run(run),
      .done(unitDone[@{counter}]),
      .clk(clk),
      .rst(rst_int)
   );
#{set counter counter + 1}
#{end}

endmodule
