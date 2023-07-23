`timescale 1ns / 1ps

// Ex: 64 bits input and 32 bits output, sel == 0 selects lower 32 bits. sel == 1 selects upper 32 bits

module WideAdapter #(
   parameter INPUT_W = 64,
   parameter OUTPUT_W = 32
   )
   (
      sel,

      input [INPUT_W-1:0] in,
      output [OUTPUT_W-1:0] out
   );

localparam DIFF = INPUT_W / OUTPUT_W;
localparam DECISION_BIT = $clog2(DIFF);

input  [DECISION_BIT-1:0] sel;

assign out = in[sel * OUTPUT_W +: OUTPUT_W];

endmodule