#{include "versat_common.tpl"}

#{set nDones #{call CountDones instances}}

`timescale 1ns / 1ps
`include "axi.vh"
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
   input [`DATAPATH_W-1:0]         m_databus_rdata,
   output [`nIO*`DATAPATH_W-1:0]   m_databus_wdata,
   output [`nIO*`DATAPATH_W/8-1:0] m_databus_wstrb,
   output [`nIO*8-1:0]             m_databus_len,
   input [`nIO-1:0]                m_databus_last,
`endif
   // data/control interface
   input                           valid,
   input [ADDR_W-1:0]              addr,
   input [DATA_W/8-1:0]            wstrb,
   input [DATA_W-1:0]              wdata,
   output                          ready,
   output reg [DATA_W-1:0]         rdata,

   #{for ext external}
      #{set i index}
      #{if ext.type}
   // DP
      #{for port 2}
   output [ADDR_W-1:0]   ext_dp_addr_@{i}_port_@{port},
   output [DATA_W-1:0]   ext_dp_out_@{i}_port_@{port},
   input  [DATA_W-1:0]   ext_dp_in_@{i}_port_@{port},
   output                ext_dp_enable_@{i}_port_@{port},
   output                ext_dp_write_@{i}_port_@{port},
      #{end}
      #{else}
   // 2P
   output [ADDR_W-1:0]   ext_2p_addr_out_@{i},
   output [ADDR_W-1:0]   ext_2p_addr_in_@{i},
   output                ext_2p_write_@{i},
   output                ext_2p_read_@{i},
   input  [DATA_W-1:0]   ext_2p_data_in_@{i},
   output [DATA_W-1:0]   ext_2p_data_out_@{i},
      #{end}
   #{end}

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

reg [31:0] read_test_counter;
reg enableReadCounter;
always @(posedge clk,posedge rst)
begin
   if(rst) begin
      read_test_counter <= 0;
      enableReadCounter <= 0;
   end else if(valid) begin
      read_test_counter <= 0;
      enableReadCounter <= 1;
   end else if(enableReadCounter) begin
      read_test_counter <= read_test_counter + 1;
   end   
end


reg [31:0] test_counter;
reg enableCounter;
always @(posedge clk,posedge rst)
begin
   if(rst) begin
      test_counter <= 0;
      enableCounter <= 0;
   end else if(run) begin
      test_counter <= 0;
      enableCounter <= 1;
   end else if(done) begin
      enableCounter <= 0;
   end else if(enableCounter) begin
      test_counter <= test_counter + 1;
   end
end

wire [31:0] latency_counter = test_counter; // When a unit is producing the correct value in the vcd, the correct versat_latency value is given by this wire

#{if unitsMapped}
assign rdata = (versat_ready ? versat_rdata : unitRdataFinal);
#{else}
assign rdata = versat_rdata;
#{end}

#{if unitsMapped}
assign ready = versat_ready | wor_ready;
#{else}
assign ready = versat_ready;
#{end}

#{if versatValues.configurationBits} reg [@{versatValues.configurationBits-1}:0] configdata; #{end}
#{if versatValues.stateBits} wire [@{versatValues.stateBits - 1}:0] statedata; #{end}

wire [@{nDones - 1}:0] unitDone;

#{if unitsMapped}
reg [@{unitsMapped - 1}:0] memoryMappedEnable;
wire[@{unitsMapped - 1}:0] unitReady;
assign wor_ready = (|unitReady);
#{end}

assign done = (&unitDone && !run);

#{if versatValues.numberConnections}
wire [31:0] #{join ", " for node instances}
   #{if node.outputs} 
      #{join ", " for j node.outputs} output_@{node.inst.id}_@{j} #{end}
   #{else}
      unused_@{node.inst.id} #{end}
#{end};
#{end}

#{if unitsMapped}
wire [31:0] unitRData[@{unitsMapped - 1}:0];
assign unitRdataFinal = (#{join "|" for i unitsMapped} unitRData[@{i}] #{end});
#{end}

// Memory mapped
#{if unitsMapped}
always @*
begin
   memoryMappedEnable = {@{unitsMapped}{1'b0}};
   if(valid & memoryMappedAddr)
   begin
   #{set counter 0}
   #{for node instances}
   #{set inst node.inst}
   #{if inst.declaration.isMemoryMapped}
      #{if versatData[counter].memoryMaskSize}
      if(addr[@{memoryAddressBits - 1}:@{memoryAddressBits - versatData[counter].memoryMaskSize}] == @{memoryAddressBits - inst.declaration.memoryMapBits}'b@{versatData[counter].memoryMask}) // @{versatBase + memoryMappedBase * 4 + inst.memMapped * 4 |> Hex}
         memoryMappedEnable[@{counter}] = 1'b1;
      #{else}
      memoryMappedEnable[0] = 1'b1;
      #{end}
   #{inc counter}
   #{end}
   #{end}
   end
end
#{end}

// Config writing
always @(posedge clk,posedge rst_int)
begin
   if(rst_int) begin
      configdata <= {@{configurationBits}{1'b0}};
   end else if(valid & we & !memoryMappedAddr) begin
      // Config
      #{set counter 0}
      #{set addr 1}
      #{for node instances}
      #{set inst node.inst}
      #{set decl inst.declaration}
      #{for wire decl.configs}
      if(addr[@{versatValues.configurationAddressBits - 1}:0] == @{addr}) // @{versatBase + addr * 4 |> Hex}
         configdata[@{counter}+:@{wire.bitsize}] <= wdata[@{wire.bitsize - 1}:0]; // @{wire.name} - @{decl.name}
      #{inc addr}
      #{set counter counter + wire.bitsize}
      #{end}
      #{end}

      // Static
      #{for unit accel.staticInfo}
      #{for wire unit.data.configs}
      if(addr[@{versatValues.configurationAddressBits - 1}:0] == @{addr}) // @{versatBase + addr * 4 |> Hex}
         configdata[@{counter}+:@{wire.bitsize}] <= wdata[@{wire.bitsize - 1}:0]; //  @{unit.id.parent.name}_@{unit.id.name}_@{wire.name}
      #{inc addr}
      #{set counter counter + wire.bitsize}
      #{end}
      #{end}

      // Delays
      #{for node instances}
      #{set inst node.inst}
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
      #{for node instances}
      #{set inst node.inst}
      #{set decl inst.declaration}
      #{for wire decl.states}
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
#{set staticDataIndex staticStart}
#{set stateDataIndex 0}
#{set delaySeen 0}
#{set ioIndex 0}
#{set externalCounter 0}
#{set memoryMappedIndex 0}
#{set doneCounter 0}
#{for node instances}
#{set inst node.inst}
#{set decl inst.declaration}
   @{decl.name} @{inst.parameters} @{inst.name |> Identify}_@{counter} (
      #{for i node.outputs}
         .out@{i}(output_@{inst.id}_@{i}),
      #{end} 

      #{for input node.inputs}
      #{if input.node}
         .in@{index}(output_@{input.node.inst.id}_@{input.port}),
      #{end}
      #{end}

      #{for wire decl.configs}
      #{if decl.type}
         .@{wire.name}(configdata[@{configDataIndex}+:@{wire.bitsize}]),
      #{else}
         .@{wire.name}(configdata[@{configDataIndex}+:@{wire.bitsize}]),
      #{end}
      #{set configDataIndex configDataIndex + wire.bitsize}
      #{end}

      #{for unit decl.staticUnits}
      #{set id unit.first}
      #{for wire unit.second.configs}
         .@{id.parent.name}_@{id.name}_@{wire.name}(configdata[@{staticDataIndex}+:@{wire.bitsize}]),
      #{set staticDataIndex staticDataIndex + wire.bitsize}
      #{end}
      #{end}

      #{for i decl.nDelays}
         .delay@{i}(configdata[@{delayStart + (delaySeen * 32)}+:32]),
      #{inc delaySeen}
      #{end}

      #{for external decl.externalMemory}
         #{set i index}
         #{if external.type}
      // DP
         #{for port 2}
      .ext_dp_addr_@{i}_port_@{port}(ext_dp_addr_@{externalCounter}_port_@{port}),
      .ext_dp_out_@{i}_port_@{port}(ext_dp_out_@{externalCounter}_port_@{port}),
      .ext_dp_in_@{i}_port_@{port}(ext_dp_in_@{externalCounter}_port_@{port}),
      .ext_dp_enable_@{i}_port_@{port}(ext_dp_enable_@{externalCounter}_port_@{port}),
      .ext_dp_write_@{i}_port_@{port}(ext_dp_write_@{externalCounter}_port_@{port}),
         #{end}
         #{else}
      // 2P
      .ext_2p_addr_out_@{i}(ext_2p_addr_out_@{externalCounter}),
      .ext_2p_addr_in_@{i}(ext_2p_addr_in_@{externalCounter}),
      .ext_2p_write_@{i}(ext_2p_write_@{externalCounter}),
      .ext_2p_read_@{i}(ext_2p_read_@{externalCounter}),
      .ext_2p_data_in_@{i}(ext_2p_data_in_@{externalCounter}),
      .ext_2p_data_out_@{i}(ext_2p_data_out_@{externalCounter}),
         #{end}
      #{inc externalCounter}
      #{end}

      #{for wire decl.states}
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
      .databus_rdata(m_databus_rdata),
      .databus_wdata(m_databus_wdata[@{ioIndex * 32} +: @{32 * decl.nIOs}]),
      .databus_wstrb(m_databus_wstrb[@{ioIndex * 4} +: @{4 * decl.nIOs}]),
      .databus_len(m_databus_len[@{ioIndex * 8} +: @{8 * decl.nIOs}]),
      .databus_last(m_databus_last[@{ioIndex} +: @{decl.nIOs}]),
      #{set ioIndex ioIndex + decl.nIOs}
      #{end} 
      
      .run(run),
      #{if decl.implementsDone}
      .done(unitDone[@{doneCounter}]),
      #{inc doneCounter}
      #{end}
      .clk(clk),
      .rst(rst_int)
   );
#{inc counter}
#{end}

endmodule
