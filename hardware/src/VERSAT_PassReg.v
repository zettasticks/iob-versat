`timescale 1ns / 1ps

// Lets data pass when valid is set and registers and outputs it when valid is not set.
// Think of it as a register that outputs its value in the same cycle that it is set.
// This is mostly used to support logic that produces data one cycle earlier than a simple sequential approach would.

module VERSAT_PassReg #(
   parameter DATA_W = 1,
   parameter RESET_VALUE = 0
) (
   input [DATA_W-1:0]  inp_data_i,
   input               inp_valid_i,

   output [DATA_W-1:0] out_data_o,

   input clk_i,
   input arst_i
);

reg [DATA_W-1:0] reg_data;

always @(posedge clk_i,posedge arst_i) begin
   if(arst_i) begin
      reg_data <= RESET_VALUE;
   end else begin
      if(inp_valid_i) begin
         reg_data <= inp_data_i;
      end
   end
end

assign out_data_o = inp_valid_i ? inp_data_i : reg_data;

endmodule