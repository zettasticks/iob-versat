`timescale 1ns / 1ps

// Unit that computes both databus address and memory address for reading and writing operations.
// This unit is the source of truth for how the address gen interface generates the addresses for accessing memory.

// Take care when changing this. Software address gen code needs to match this logic
module SuperAddress #(
   parameter AXI_ADDR_W = 32,
   parameter DATA_W   = 32,
   parameter ADDR_W   = 10,
   parameter PERIOD_W = 10,
   parameter DELAY_W  = 7,
   parameter LEN_W      = 8,
   parameter COUNT_W    = 8
) (
   input clk_i,
   input rst_i,

   input run_i,

   input ignore_first_i, // Used to align when using duty expressions

   //configurations 
   input        [  ADDR_W - 1:0] start_i,
   input        [PERIOD_W - 1:0] duty_i,

   input        [PERIOD_W - 1:0] per_i,
   input signed [  ADDR_W - 1:0] incr_i,

   input        [  ADDR_W - 1:0] iter_i,
   input signed [  ADDR_W - 1:0] shift_i,

   input        [PERIOD_W - 1:0] per2_i,
   input signed [  ADDR_W - 1:0] incr2_i,

   input        [  ADDR_W - 1:0] iter2_i,
   input signed [  ADDR_W - 1:0] shift2_i,

   input        [PERIOD_W - 1:0] per3_i,
   input signed [  ADDR_W - 1:0] incr3_i,

   input        [  ADDR_W - 1:0] iter3_i,
   input signed [  ADDR_W - 1:0] shift3_i,

   input        [PERIOD_W - 1:0] per4_i,
   input signed [  ADDR_W - 1:0] incr4_i,

   input        [  ADDR_W - 1:0] iter4_i,
   input signed [  ADDR_W - 1:0] shift4_i,

   input        [ DELAY_W - 1:0] delay_i,

   input        [  ADDR_W - 1:0] work_i,
   input        [  ADDR_W - 1:0] workSize_i,

   //outputs 
   output reg                valid_o,
   output                    insideDuty_o,
   input                     ready_i,
   output reg [ADDR_W - 1:0] addr_o,
   output                    store_o,

   output wire doneDatabus,
   output reg  doneAddress,
   output wire done_o,

   input            [COUNT_W-1:0] count_i,
   input       [  AXI_ADDR_W-1:0] start_address_i,
   input       [  AXI_ADDR_W-1:0] address_shift_i,
   input       [       LEN_W-1:0] databus_length,

   input                          data_ready_i,
   input                          data_valid_i,
   input                          reading,

   // Only address databus values. Read vs Write implemented outside of this unit.
   input                          databus_ready,
   output                         databus_valid,
   output     [  AXI_ADDR_W-1:0]  databus_addr,
   output     [       LEN_W-1:0]  databus_len,
   input                          databus_last
);

localparam OFFSET_W = $clog2(DATA_W / 8);

reg                                           [   DELAY_W-1:0] delay_counter;

reg                                           [  ADDR_W - 1:0] iter,iter2,iter3,iter4;
reg                                           [PERIOD_W - 1:0] per,per2,per3,per4;

reg [ADDR_W-1:0] addr2,addr3,addr4;

wire iter4Cond = (((iter4 + 1) == iter4_i) || (iter4_i == 0));
wire per4Cond = (((per4 + 1) == per4_i) || (per4_i == 0));

wire iter3Cond = (((iter3 + 1) == iter3_i) || (iter3_i == 0));
wire per3Cond = (((per3 + 1) == per3_i) || (per3_i == 0));

wire iter2Cond = (((iter2 + 1) == iter2_i) || (iter2_i == 0));
wire per2Cond = (((per2 + 1) == per2_i) || (per2_i == 0));

//wire true_iter = (ignore ? iter_i + 1 : iter_i);

wire iterCond = (((iter + 1) == iter_i) || (iter_i == 0));
wire perCond = (((per + 1) == per_i) || (per_i == 0));

wire [7:0] cases = {iter4Cond,per4Cond,iter3Cond,per3Cond,iter2Cond,per2Cond,iterCond,perCond};

reg [ADDR_W-1:0] work;

assign insideDuty_o = (valid_o && per <= duty_i && !ignore && (work_i == 0 || workSize_i == 0 || work < work_i));
assign store_o      = (valid_o && per < duty_i  && !ignore);

/* TODO:
   
   The address generation can be decoupled, but it is better if we separate it into individual modules.
   Because we must take into account future iter and per conditions.
   The idea of "runnning one ahead" does not work if we only take into account per. 

   Basically need a module that implements per and iter logic only. We can then implement three of them if needed

*/

reg ignore;

wire isZero = (per_i == 0);

always @(posedge clk_i,posedge rst_i) begin
   if (rst_i) begin
      delay_counter <= 0;
      addr_o        <= 0;
      addr2         <= 0;
      addr3         <= 0;
      addr4         <= 0;
      iter          <= 0;
      iter2         <= 0;
      iter3         <= 0;
      iter4         <= 0;
      per           <= 0;
      per2          <= 0;
      per3          <= 0;
      per4          <= 0;
      work          <= 0;
      valid_o       <= 0;
      doneAddress   <= 1'b1;
   end else if (run_i) begin
      delay_counter <= delay_i;
      addr_o        <= (start_i << OFFSET_W);
      addr2         <= (start_i << OFFSET_W);
      addr3         <= (start_i << OFFSET_W);
      addr4         <= (start_i << OFFSET_W);
      iter          <= 0;
      iter2         <= 0;
      iter3         <= 0;
      iter4         <= 0;
      per           <= 0;
      per2          <= 0;
      per3          <= 0;
      per4          <= 0;
      valid_o       <= 0;
      work          <= 0;
      doneAddress   <= 1'b0;
      ignore        <= ignore_first_i;
      if (delay_i == 0) begin
         if(isZero) begin
            doneAddress <= 1'b1;
         end else begin
            valid_o <= 1'b1;
         end
      end
   end else if (|delay_counter) begin
      delay_counter <= delay_counter - 1;
      work <= 0;
      if(delay_counter == 1) begin
         if(isZero) begin
            doneAddress <= 1'b1;
         end else begin
            valid_o <= 1'b1;
         end
      end
   end else if (valid_o && ready_i) begin
      work <= work + 1;
      if(work < work_i || work_i == 0 || workSize_i == 0) begin
         casez(cases)
         8'b???????0: begin
            if (per < duty_i && (!ignore)) begin
               addr_o <= addr_o + (incr_i << OFFSET_W);
            end
            per <= per + 1;
         end
         8'b??????01: begin
            if(!ignore)
               addr_o <= addr_o + (shift_i << OFFSET_W);
            per    <= 0;
            ignore <= 0;
            iter   <= iter + 1;
         end
         8'b?????011: begin
            if(!ignore) begin
               addr_o <= addr2 + (incr2_i << OFFSET_W);
               addr2  <= addr2 + (incr2_i << OFFSET_W);
            end
            per    <= 0;
            iter   <= 0;
            ignore <= 0;
            per2   <= per2 + 1;
         end
         8'b????0111: begin
            if(!ignore) begin
               addr_o <= addr2 + (shift2_i << OFFSET_W);
               addr2  <= addr2 + (shift2_i << OFFSET_W);
            end
            per    <= 0;
            iter   <= 0;
            per2   <= 0;
            ignore <= 0;
            iter2  <= iter2 + 1;
         end
         8'b???01111: begin
            if(!ignore) begin
               addr_o <= addr3 + (incr3_i << OFFSET_W);
               addr2  <= addr3 + (incr3_i << OFFSET_W);
               addr3  <= addr3 + (incr3_i << OFFSET_W);
            end
            per    <= 0;
            iter   <= 0;
            per2   <= 0;
            iter2  <= 0;
            ignore <= 0;
            per3   <= per3 + 1;
         end
         8'b??011111: begin
            if(!ignore) begin
               addr_o <= addr3 + (shift3_i << OFFSET_W);
               addr2  <= addr3 + (shift3_i << OFFSET_W);
               addr3  <= addr3 + (shift3_i << OFFSET_W);
            end
            per    <= 0;
            iter   <= 0;
            per2   <= 0;
            iter2  <= 0;
            per3   <= 0;
            ignore <= 0;
            iter3  <= iter3 + 1;
         end

         8'b?0111111: begin
            if(!ignore) begin
               addr_o <= addr4 + (incr4_i << OFFSET_W);
               addr2  <= addr4 + (incr4_i << OFFSET_W);
               addr3  <= addr4 + (incr4_i << OFFSET_W);
               addr4  <= addr4 + (incr4_i << OFFSET_W);
            end
            per    <= 0;
            iter   <= 0;
            per2   <= 0;
            iter2  <= 0;
            per3   <= 0;
            iter3  <= 0;
            ignore <= 0;
            per4   <= per4 + 1;
         end
         8'b01111111: begin
            if(!ignore) begin
               addr_o <= addr4 + (incr4_i << OFFSET_W);
               addr2  <= addr4 + (incr4_i << OFFSET_W);
               addr3  <= addr4 + (incr4_i << OFFSET_W);
               addr4  <= addr4 + (incr4_i << OFFSET_W);
            end
            per    <= 0;
            iter   <= 0;
            per2   <= 0;
            iter2  <= 0;
            per3   <= 0;
            iter3  <= 0;
            per4   <= 0;         
            ignore <= 0;
            iter4  <= iter4 + 1;
         end

         8'b11111111: begin
            doneAddress <= 1'b1;
            valid_o <= 0;
         end
         endcase
      end
      if(work + 1 >= workSize_i) begin
         work <= 0;
      end

   end
end

// Databus side
reg [1:0] state;
reg [  AXI_ADDR_W-1:0] address;

assign databus_len   = databus_length;
assign databus_addr = address;

wire count_is_zero;
wire counter_advance;

CountNLoops #(.COUNT_W(COUNT_W)) counter(
.start_i(run_i),
.count_i(count_i),

.advance_i(counter_advance),

.count_is_zero_o(count_is_zero),

.clk_i(clk_i),
.rst_i(rst_i)
);

assign doneDatabus = (state == 2'b0);

reg databus_valid_reg; // Allows data_ready to propagate
assign databus_valid = (state == 2'b10) && (reading ? (databus_valid_reg && data_ready_i) : data_valid_i);

always @(posedge clk_i,posedge rst_i) begin
   if(rst_i) begin
      state <= 0;
      address <= 0;
      databus_valid_reg <= 1'b0;
   end else begin
      case(state)
      2'b00: begin
         if(run_i && count_i != 0 && databus_length != 0) begin
            state <= 2'b01;
            address <= start_address_i;
         end
      end
      2'b01: begin
         if(count_is_zero) begin
            state <= 2'b00;
         end else begin
            state <= 2'b10;
            databus_valid_reg <= 1'b1;
         end
      end
      2'b10: begin
         if(databus_valid && databus_ready && databus_last) begin
            databus_valid_reg <= 1'b0;
            address <= address + address_shift_i;
            state <= 2'b01;
         end
      end
      2'b11: begin
      end
      endcase
   end
end

assign counter_advance = (databus_valid && databus_ready && databus_last);

assign done_o = doneAddress && doneDatabus;

endmodule  // SuperAddress
