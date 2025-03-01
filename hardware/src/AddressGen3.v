`timescale 1ns / 1ps

module AddressGen3 #(
   parameter ADDR_W   = 10,
   parameter PERIOD_W = 10,
   parameter DELAY_W  = 7,
   parameter DATA_W   = 8
) (
   input clk_i,
   input rst_i,

   input run_i,

   input ignore_first_i, // Treat as this is bias vread, for now

   //configurations 
   input        [  ADDR_W - 1:0] start_i,
   input        [PERIOD_W - 1:0] duty_i,

   input        [PERIOD_W - 1:0] period_i,
   input signed [  ADDR_W - 1:0] incr_i,

   input        [  ADDR_W - 1:0] iterations_i,
   input signed [  ADDR_W - 1:0] shift_i,

   input        [PERIOD_W - 1:0] period2_i,
   input signed [  ADDR_W - 1:0] incr2_i,

   input        [  ADDR_W - 1:0] iterations2_i,
   input signed [  ADDR_W - 1:0] shift2_i,

   input        [PERIOD_W - 1:0] period3_i,
   input signed [  ADDR_W - 1:0] incr3_i,

   input        [  ADDR_W - 1:0] iterations3_i,
   input signed [  ADDR_W - 1:0] shift3_i,

   input        [ DELAY_W - 1:0] delay_i,

   //outputs 
   output                valid_o,
   input                 ready_i,
   output [ADDR_W - 1:0] addr_o,
   output                store_o,

   output  done_o
);

   SuperAddress #(
      .ADDR_W(ADDR_W),
      .PERIOD_W(PERIOD_W),
      .DELAY_W(DELAY_W),
      .DATA_W(DATA_W),
      .LEN_W(1),
      .COUNT_W(1),
      .AXI_ADDR_W(1)
      ) reader (
      .clk_i(clk_i),
      .rst_i(rst_i),
      .run_i(run_i),
      .done_o(done_o),

      .ignore_first_i(ignore_first_i),

      //configurations 
      .period_i(period_i),
      .delay_i (delay_i),
      .start_i (start_i),
      .incr_i  (incr_i),

      .iterations_i(iterations_i),
      .duty_i      (duty_i),
      .shift_i     (shift_i),

      .period2_i(period2_i),
      .incr2_i(incr2_i),
      .iterations2_i(iterations2_i),
      .shift2_i(shift2_i),

      .period3_i(period3_i),
      .incr3_i(incr3_i),
      .iterations3_i(iterations3_i),
      .shift3_i(shift3_i),

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

      .count_i(1'b0),
      .start_address_i(1'b0),
      .address_shift_i(1'b0),
      .databus_length(1'b0)
   );

endmodule  // MyAddressGen
