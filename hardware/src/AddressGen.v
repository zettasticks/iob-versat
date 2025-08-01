`timescale 1ns / 1ps

module AddressGen #(
   parameter ADDR_W   = 10,
   parameter PERIOD_W = 10,
   parameter DELAY_W  = 7,
   parameter DATA_W   = 32
) (
   input clk_i,
   input rst_i,

   input run_i,

   //configurations 
   input        [  ADDR_W - 1:0] start_i,
   input        [PERIOD_W - 1:0] duty_i,

   input        [PERIOD_W - 1:0] per_i,
   input signed [  ADDR_W - 1:0] incr_i,

   input        [  ADDR_W - 1:0] iter_i,
   input signed [  ADDR_W - 1:0] shift_i,


   input        [ DELAY_W - 1:0] delay_i,

   //outputs 
   output                    valid_o,
   input                     ready_i,
   output reg [ADDR_W - 1:0] addr_o,
   output                    store_o,

   output reg done_o
);

wire [PERIOD_W-1:0] fullZeroPeriod = 0; // To remove warning about bitsize
wire [ADDR_W-1:0] fullZeroIter = 0; // To remove warning about bitsize

   SuperAddress #(
      .ADDR_W(ADDR_W),
      .PERIOD_W(PERIOD_W),
      .DELAY_W(DELAY_W),
      .DATA_W(DATA_W)
      ) reader (
      .clk_i(clk_i),
      .rst_i(rst_i),
      .run_i(run_i),
      .done_o(done_o),

      .ignore_first_i(1'b0),

      //configurations 
      .per_i(per_i),
      .delay_i (delay_i),
      .start_i (start_i),
      .incr_i  (incr_i),

      .iter_i(iter_i),
      .duty_i      (duty_i),
      .shift_i     (shift_i),

      .per2_i(fullZeroPeriod),
      .incr2_i(fullZeroIter),
      .iter2_i(fullZeroIter),
      .shift2_i(fullZeroIter),

      .per3_i(fullZeroPeriod),
      .incr3_i(fullZeroIter),
      .iter3_i(fullZeroIter),
      .shift3_i(fullZeroIter),

      .doneDatabus(),
      .doneAddress(),

      //outputs 
      .valid_o(valid_o),
      .ready_i(ready_i),
      .addr_o (addr_o),
      .store_o(store_o),

      .databus_ready(1'b1),
      .databus_valid(),
      .databus_addr(),
      .databus_len(),
      .databus_last(1'b1),

      // Data interface
      .data_valid_i(1'b1),
      .data_ready_i(1'b1),
      .reading(1'b1),
      .data_last_o(),

      .count_i(0),
      .start_address_i(0),
      .address_shift_i(0),
      .databus_length(0)
   );

endmodule  // MyAddressGen
