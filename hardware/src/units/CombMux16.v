`timescale 1ns / 1ps

module CombMux16 #(
   parameter DATA_W = 32
) (
   //input / output data
   input [DATA_W-1:0] in0,
   input [DATA_W-1:0] in1,
   input [DATA_W-1:0] in2,
   input [DATA_W-1:0] in3,
   input [DATA_W-1:0] in4,
   input [DATA_W-1:0] in5,
   input [DATA_W-1:0] in6,
   input [DATA_W-1:0] in7,
   input [DATA_W-1:0] in8,
   input [DATA_W-1:0] in9,
   input [DATA_W-1:0] in10,
   input [DATA_W-1:0] in11,
   input [DATA_W-1:0] in12,
   input [DATA_W-1:0] in13,
   input [DATA_W-1:0] in14,
   input [DATA_W-1:0] in15,

   (* versat_latency = 0 *) output reg [DATA_W-1:0] out0,

   input [3:0] sel
);

   always @* begin
      case (sel)
         4'b0000: out0 = in0;
         4'b0001: out0 = in1;
         4'b0010: out0 = in2;
         4'b0011: out0 = in3;
         4'b0100: out0 = in4;
         4'b0101: out0 = in5;
         4'b0110: out0 = in6;
         4'b0111: out0 = in7;
         4'b1000: out0 = in8;
         4'b1001: out0 = in9;
         4'b1010: out0 = in10;
         4'b1011: out0 = in11;
         4'b1100: out0 = in12;
         4'b1101: out0 = in13;
         4'b1110: out0 = in14;
         4'b1111: out0 = in15;
      endcase
   end

endmodule
