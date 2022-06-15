`timescale 1ns / 1ps
`include "axi.vh"
//`include "xversat.vh"
//`include "xdefs.vh"
`include "versat_defs.vh"

module versat_instance #(
      parameter ADDR_W = `ADDR_W,
      parameter DATA_W = `DATA_W,
      parameter AXI_ADDR_W = `AXI_ADDR_W
   )
   (
   // Databus master interface
`ifdef nIO
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

reg [@{versatValues.configurationBits + versatValues.nDelays * 32 - 1}:0] configdata;
wire [@{versatValues.stateBits - 1}:0] statedata;

wire [@{numberUnits - 1}:0] unitDone;
reg [@{unitsMapped - 1}:0] memoryMappedEnable;
wire[@{unitsMapped - 1}:0] unitReady;

assign wor_ready = (|unitReady);
assign done = &unitDone;

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
   #{if inst.declaration.memoryMapDWords}
      if(addr[@{memoryAddressBits - 1}:@{memoryAddressBits - inst.versatData.memoryMaskSize}] == @{inst.versatData.memoryMaskSize}'b@{inst.versatData.memoryMask})
         memoryMappedEnable[@{counter}] = 1'b1;
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
      #{set addr 0}
      #{for inst instances}
      #{set decl inst.declaration}
      #{for i decl.nConfigs}
      #{set wire decl.configWires[i]}
      if(addr[@{configAddressRangeHigh}:@{configAddressRangeLow}] == @{addr + 1})
         configdata[@{counter}+:@{wire.bitsize}] <= wdata[@{wire.bitsize - 1}:0]; // @{wire.name}
      #{inc addr}
      #{set counter counter + wire.bitsize}
      #{end}
      #{for i decl.nDelays}
      if(addr[@{configAddressRangeHigh}:@{configAddressRangeLow}] == @{addr + 1})
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
      if(addr[@{stateAddressRangeHigh}:@{stateAddressRangeLow}] == @{addr + 1})
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

      #{if decl.memoryMapDWords}
      .valid(memoryMappedEnable[@{memoryMappedIndex}]),
      .wstrb(wstrb),
      .addr(addr[@{inst.versatData.addressTopBit}:0]),
      .rdata(unitRData[@{memoryMappedIndex}]),
      .ready(unitReady[@{memoryMappedIndex}]),
      .wdata(wdata),
      #{inc memoryMappedIndex}
      #{end}

      #{for i decl.nIOs}
      .databus_ready(m_databus_ready[@{ioIndex}]),
      .databus_valid(m_databus_valid[@{ioIndex}]),
      .databus_addr(m_databus_addr[@{ioIndex * 32}+:32]),
      .databus_rdata(m_databus_rdata[@{ioIndex * 32}+:32]),
      .databus_wdata(m_databus_wdata[@{ioIndex * 32}+:32]),
      .databus_wstrb(m_databus_wstrb[@{ioIndex * 4}+:4]),
      #{inc ioIndex}
      #{end} 
      
      .run(run),
      .done(unitDone[@{counter}]),
      .clk(clk),
      .rst(rst_int)
   );

#{set counter counter + 1}
#{end}

endmodule