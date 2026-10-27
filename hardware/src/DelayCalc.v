`timescale 1ns / 1ps

module DelayCalc #(
   parameter DELAY_W = 7
) (
   //control
   input clk,
   input rst,

   input      running,
   input      run,
   output reg done,

   output     lastCycle,

   input [DELAY_W-1:0] delay
);
   reg [DELAY_W-1:0] delay_reg;

   assign lastCycle = (delay_reg == 0) && !done;

   always @(posedge clk, posedge rst) begin
      if (rst) begin
         delay_reg  <= 0;
         done <= 1'b1;
      end else if(run) begin
         delay_reg <= delay;
         done <= 1'b0;
      end else if(running) begin
         if (delay_reg == 0) begin
            done <= 1'b1;
         end else begin
            delay_reg <= delay_reg - 1;
         end
      end
   end

endmodule
