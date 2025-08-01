`timescale 1ns / 1ps

`define COMPLEX_INTERFACE

(* source *) module Mem #(
   parameter DATA_W        = 32,
   parameter SIZE_W        = 32,
   parameter DELAY_W       = 7,
   parameter ADDR_W        = 12
) (
   //control
   input clk,
   input rst,

   input  running,
   input  run,
   output done,
   input  disabled,

   //databus interface
   input      [DATA_W/8-1:0] wstrb,
   input      [  ADDR_W-1:0] addr,
   input      [  DATA_W-1:0] wdata,
   input                     valid,
   output reg                rvalid,
   output     [  DATA_W-1:0] rdata,

   //input / output data
                            input  [DATA_W-1:0] in0,
                            input  [DATA_W-1:0] in1,
   (* versat_latency = 3 *) output [DATA_W-1:0] out0,
   (* versat_latency = 3 *) output [DATA_W-1:0] out1,

   // External memory
   output [ADDR_W-1:0] ext_dp_addr_0_port_0,
   output [DATA_W-1:0] ext_dp_out_0_port_0,
   input  [DATA_W-1:0] ext_dp_in_0_port_0,
   output              ext_dp_enable_0_port_0,
   output              ext_dp_write_0_port_0,

   output [ADDR_W-1:0] ext_dp_addr_0_port_1,
   output [DATA_W-1:0] ext_dp_out_0_port_1,
   input  [DATA_W-1:0] ext_dp_in_0_port_1,
   output              ext_dp_enable_0_port_1,
   output              ext_dp_write_0_port_1,

   // Configuration
   input [ADDR_W-1:0] iterA,
   input [       9:0] perA,
   input [       9:0] dutyA,
   input [ADDR_W-1:0] startA,
   input [ADDR_W-1:0] shiftA,
   input [ADDR_W-1:0] incrA,
   input              reverseA,
   input              extA,
   input              in0_wr,
   input [ADDR_W-1:0] iter2A,
   input [       9:0] per2A,
   input [ADDR_W-1:0] shift2A,
   input [ADDR_W-1:0] incr2A,

   input [ADDR_W-1:0] iterB,
   input [       9:0] perB,
   input [       9:0] dutyB,
   input [ADDR_W-1:0] startB,
   input [ADDR_W-1:0] shiftB,
   input [ADDR_W-1:0] incrB,
   input              reverseB,
   input              extB,
   input              in1_wr,
   input [ADDR_W-1:0] iter2B,
   input [       9:0] per2B,
   input [ADDR_W-1:0] shift2B,
   input [ADDR_W-1:0] incr2B,

   input [DELAY_W-1:0] delay0,
   input [DELAY_W-1:0] delay1
);

   wire we = |wstrb;

   wire doneA, doneB;

   //output databus
   wire [DATA_W-1:0] outA, outB;
   reg [DATA_W-1:0] outA_reg, outB_reg;

   reg [DELAY_W-1:0] testDelay0;
   always @(posedge clk, posedge rst) begin
      if (rst) begin
         testDelay0 <= 0;
      end else if (run) begin
         testDelay0 <= delay0 + 3;
      end else if (|testDelay0) begin
         testDelay0 <= testDelay0 - 1;
      end
   end

   reg [DELAY_W-1:0] testDelay1;
   always @(posedge clk, posedge rst) begin
      if (rst) begin
         testDelay1 <= 0;
      end else if (run) begin
         testDelay1 <= delay1 + 3;
      end else if (|testDelay1) begin
         testDelay1 <= testDelay1 - 1;
      end
   end

   assign out0 = (running & (|testDelay0) == 0) ? outA_reg : 0;
   assign out1 = (running & (|testDelay1) == 0) ? outB_reg : 0;

   // Delay done by 3 cycles so that pc-emul matches simulation
   reg doneA_1,doneB_1;
   reg doneA_2,doneB_2;
   reg doneA_3,doneB_3;
   always @(posedge clk,posedge rst)
   begin
      if(rst) begin
         doneA_1 <= 0;
         doneA_2 <= 0;
         doneB_1 <= 0;
         doneB_2 <= 0;
      end else begin
         doneA_1 <= doneA;
         doneA_2 <= doneA_1;
         doneA_3 <= doneA_2;
         doneB_1 <= doneB;
         doneB_2 <= doneB_1;
         doneB_3 <= doneB_2;
      end
   end
   assign done = (doneA & doneB & doneA_2 & doneB_2 & doneA_3 & doneB_3);

   //assign done = (doneA & doneB);

   //function to reverse bits
   function [ADDR_W-1:0] reverseBits;
      input [ADDR_W-1:0] word;
      integer i;

      begin
         for (i = 0; i < ADDR_W; i = i + 1) reverseBits[i] = word[ADDR_W-1-i];
      end
   endfunction

   //mem enables output by addr gen
   wire enA_int;
   wire enA = enA_int | valid;
   wire enB;

   //write enables
   wire wrA = valid? we : (enA_int & in0_wr & ~extA); //addrgen on & input on & input isn't address
   wire wrB = (enB & in1_wr & ~extB);

   //port addresses and enables
   wire [ADDR_W-1:0] addrA, addrA_int, addrA_int2;
   wire [ADDR_W-1:0] addrB, addrB_int, addrB_int2;

   //data inputs
   wire [DATA_W-1:0] data_to_wrA = valid ? wdata : in0;

   //address generators

   AddressGen #(
      .ADDR_W(ADDR_W),
      .DATA_W(SIZE_W),
      .DELAY_W(DELAY_W)
   ) addrgenA (
      .clk_i(clk),
      .rst_i(rst),
      .run_i(run && !disabled),

      //configurations 
      .per_i(perA),
      .start_i (startA),
      .incr_i  (incrA),
      .delay_i (delay0),

`ifdef COMPLEX_INTERFACE
      .iter_i(iterA),
      .duty_i      (dutyA),
      .shift_i     (shiftA),
`endif

      //outputs 
      .valid_o(enA_int),
      .ready_i(1'b1),
      .addr_o (addrA_int),
      .store_o(),

      .done_o (doneA)
   );

   AddressGen #(
      .ADDR_W(ADDR_W),
      .DATA_W(SIZE_W),
      .DELAY_W(DELAY_W)
   ) addrgenB (
      .clk_i(clk),
      .rst_i(rst),
      .run_i(run && !disabled),

      //configurations 
      .per_i(perB),
      .start_i (startB),
      .incr_i  (incrB),
      .delay_i (delay1),

`ifdef COMPLEX_INTERFACE
      .iter_i(iterB),
      .duty_i      (dutyB),
      .shift_i     (shiftB),
`endif

      //outputs 
      .valid_o(enB),
      .ready_i(1'b1),
      .addr_o (addrB_int),
      .store_o(),

      .done_o (doneB)
   );

   //define addresses based on ext and rvrs
   assign addrA      = valid ? addr[ADDR_W-1:0] : (extA ? in0[ADDR_W-1:0] : addrA_int2[ADDR_W-1:0]);
   assign addrB      = (extB ? in1[ADDR_W-1:0] : addrB_int2[ADDR_W-1:0]);
   assign addrA_int2 = reverseA ? reverseBits(addrA_int) : addrA_int;
   assign addrB_int2 = reverseB ? reverseBits(addrB_int) : addrB_int;

   //register mem inputs
   reg [DATA_W-1:0] data_a_reg, data_b_reg;
   reg [ADDR_W-1:0] addr_a_reg, addr_b_reg;
   reg en_a_reg, en_b_reg, we_a_reg, we_b_reg;
   always @(posedge clk) begin
      data_a_reg <= data_to_wrA;
      data_b_reg <= in1;
      addr_a_reg <= addrA;
      addr_b_reg <= addrB;
      en_a_reg   <= enA;
      en_b_reg   <= enB;
      we_a_reg   <= wrA;
      we_b_reg   <= wrB;
   end

   // Read 
   assign rdata = (rvalid ? outA_reg : 32'h0);

   // Delay by a cycle to match memory read latency
   reg readCounter[1:0];
   always @(posedge clk, posedge rst) begin
      if (rst) begin
         rvalid         <= 1'b0;
         readCounter[0] <= 1'b0;
         readCounter[1] <= 1'b0;
      end else begin
         if (rvalid) begin
            rvalid         <= 1'b0;
            readCounter[0] <= 1'b0;
            readCounter[1] <= 1'b0;
         end else begin
            // Write
            //if(valid && we)
            //   ready <= 1'b1;

            // Read
            rvalid         <= readCounter[0];
            readCounter[0] <= readCounter[1];
            if (wstrb == 0) readCounter[1] <= valid;
         end
      end
   end

   assign ext_dp_addr_0_port_0   = addr_a_reg;
   assign ext_dp_out_0_port_0    = data_a_reg;
   assign ext_dp_enable_0_port_0 = en_a_reg;
   assign ext_dp_write_0_port_0  = we_a_reg;

   assign ext_dp_addr_0_port_1   = addr_b_reg;
   assign ext_dp_out_0_port_1    = data_b_reg;
   assign ext_dp_enable_0_port_1 = en_b_reg;
   assign ext_dp_write_0_port_1  = we_b_reg;

   //register mem outputs
   always @(posedge clk, posedge rst) begin
      if (rst) begin
         outA_reg <= 0;
         outB_reg <= 0;
      end else if (run) begin
         outA_reg <= 0;
         outB_reg <= 0;
      end else begin
         outA_reg <= ext_dp_in_0_port_0;  //outA;
         outB_reg <= ext_dp_in_0_port_1;
      end
   end

endmodule

`undef COMPLEX_INTERFACE
