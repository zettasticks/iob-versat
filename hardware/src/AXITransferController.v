`timescale 1ns / 1ps

// Module that implements AXI transfer logic. Handles multiple transfers, initial and final strobes and 4k boundary. 
// Does not contain any logic to drive a transfer. It just computes values that are necessary according to the AXI specification
// A transfer is equal to a N amount of AXI bursts intended to transfer a variable amount of bytes.
// TODO: The interface logic is a bit clubersome to use by outside modules. Would be nice to give another look.
// TODO: Also take a look at the BurstAlign and BurstSplit interfaces. These three modules are usually used together but the interface is a bit clubersome when doing so.
//       It should be easy to instantiate a TransferController and the necessary Align or Split interfaces without much hassle
// TODO: We might also consider implementing some transfer logic inside this. Forcing the outside code to set the burst_start_i wire is a bit error prone.
//       We can always receive the AXI wires and use them to drive the logic. It probably does not add any extra resources since we are just implementing what the outside code is implementing already.
module AXITransferController #(
   parameter AXI_ADDR_W = 32,
   parameter AXI_DATA_W = 32,
   parameter AXI_LEN_W  = 8,
   parameter LEN_W      = 32
) (
   input [AXI_ADDR_W-1:0] address_i,  // Must remain constant during transfer
   input [     LEN_W-1:0] length_i,   // In bytes

   input                         transfer_start_i, // Assert one cycle before starting a transfer. Set address_i and length_i to the correct values beforehand
   input                         burst_start_i, // At the start of a burst (after values accepted by axi), assert this signal for one cycle. This updates the stored values and prepares the values of the next cycle

   output reg [AXI_ADDR_W-1:0] true_axi_axaddr_o,  /* registered */

   // All these signals are combinatorial. Register them outside this module if needed
   output reg [(AXI_DATA_W/8)-1:0] initial_strb_o, // First strobe of the transfer. The rest is always full 1
   output reg [(AXI_DATA_W/8)-1:0] final_strb_o, // Last strobe of the transfer. Only valid if last_transfer is asserted

   output [31:0] symbolsToRead_o,  // How many symbols (data transfers) to expect from the source

   output reg [AXI_LEN_W-1:0] true_axi_axlen_o,
   output reg                 last_transfer_o,
   output                     last_transfer_next_o,

   input clk_i,
   input rst_i
);

   function integer calculate_AXI_OFFSET_W(input integer axi_data_w);
      begin
         calculate_AXI_OFFSET_W = $clog2((axi_data_w / 8));
      end
   endfunction

   function [2:0] calculate_AXI_AXSIZE(input integer axi_data_w);
      begin
         calculate_AXI_AXSIZE = 0;
         if (axi_data_w >= 8) calculate_AXI_AXSIZE = 3'b000;
         if (axi_data_w >= 16) calculate_AXI_AXSIZE = 3'b001;
         if (axi_data_w >= 32) calculate_AXI_AXSIZE = 3'b010;
         if (axi_data_w >= 64) calculate_AXI_AXSIZE = 3'b011;
         if (axi_data_w >= 128) calculate_AXI_AXSIZE = 3'b100;
         if (axi_data_w >= 256) calculate_AXI_AXSIZE = 3'b101;
         if (axi_data_w >= 512) calculate_AXI_AXSIZE = 3'b110;
         if (axi_data_w >= 1024) calculate_AXI_AXSIZE = 3'b111;
      end
   endfunction

   localparam OFFSET_W = calculate_AXI_OFFSET_W(AXI_DATA_W);
   localparam STROBE_W = AXI_DATA_W / 8;
   localparam MAX_LENGTH = (2 ** AXI_LEN_W);

   wire [AXI_DATA_W-1:0] OFFSET_MASK = (0 | {OFFSET_W{1'b1}});

   function [15:0] min(input [15:0] a, b);
      begin
         min = (a < b) ? a : b;
      end
   endfunction

   // State variables
   reg [LEN_W-1:0] stored_len;
   reg first_i_transfer;

   // Generic variables outside generate
   reg [LEN_W-1:0] transfer_byte_size;
   reg [LEN_W-1:0] last_transfer_o_len;

   wire [12:0] max_transfer_len;  // Assigned inside the generate
   wire [12:0] max_transfer_len_minus_one;  // Assigned inside the generate

   wire [12:0] first_i_transfer_len = (max_transfer_len_minus_one + (STROBE_W - address_i[OFFSET_W-1:0]));
   wire [12:0] boundary_transfer_len = (13'h1000 - true_axi_axaddr_o[11:0]);
   wire [AXI_ADDR_W-1:0] true_address_i = address_i & (~OFFSET_MASK);

   assign last_transfer_next_o = (transfer_byte_size == stored_len);

   always @* begin
      if (first_i_transfer)
         transfer_byte_size = min(boundary_transfer_len, min(stored_len, first_i_transfer_len));
      else transfer_byte_size = min(boundary_transfer_len, min(stored_len, max_transfer_len));

      if (last_transfer_next_o) true_axi_axlen_o = last_transfer_o_len;
      else true_axi_axlen_o = (transfer_byte_size - 1) >> OFFSET_W;
   end

   reg [$clog2(STROBE_W):0] final_strb_o_count;  // Values for 128, right?
   reg [$clog2(STROBE_W):0] initial_strb_o_count;
   reg [$clog2(STROBE_W)-1:0] true_final_strb_o_count;  // $clog2(16) = 4.
   reg [$clog2(STROBE_W)-1:0] initial_strb_o_last_zero;
   reg [STROBE_W-1:0] temp_initial_strb_o;
   reg [STROBE_W-1:0] base_strb;
   wire [OFFSET_W:0] length_iPlusAddress_i = length_i[OFFSET_W:0] + address_i[OFFSET_W:0];

   integer i;
   always @* begin
      // Strobe for a address_i == 0 and length_i smaller then a full transfer
      base_strb = 0;
      for (i = 0; i < (AXI_DATA_W / 8); i = i + 1) begin
         if (i < length_i[OFFSET_W-1:0]) base_strb[i] = 1'b1;
      end

      if (length_i >= ((1 << OFFSET_W) - address_i[OFFSET_W-1:0])) begin
         initial_strb_o       = (~0) << address_i[OFFSET_W-1:0];
         initial_strb_o_count = ((1 << OFFSET_W) - address_i[OFFSET_W-1:0]);
      end else begin
         initial_strb_o       = base_strb << address_i[OFFSET_W-1:0];
         initial_strb_o_count = (1 << OFFSET_W);
      end

      final_strb_o_count      = {1'b1, length_i[OFFSET_W-1:0]} - initial_strb_o_count;
      true_final_strb_o_count = final_strb_o_count[OFFSET_W-1:0];

      if (true_final_strb_o_count == 0) begin
         final_strb_o = ~0;
      end else begin
         final_strb_o = 0;
         for (i = 0; i < (AXI_DATA_W / 8); i = i + 1) begin
            if (i < true_final_strb_o_count) final_strb_o[i] = 1'b1;
         end
      end
   end

   generate
      if (AXI_DATA_W == 32) begin
         assign symbolsToRead_o            = (|length_i[1:0]) ? (length_i >> 2) + 1 : length_i >> 2;
         assign max_transfer_len           = min(MAX_LENGTH * 4,13'h0400);
         assign max_transfer_len_minus_one = min((MAX_LENGTH * 4) - 4,13'h03FC);
         always @* begin
            last_transfer_o_len = 0;

            if ((address_i[1:0] == 2'b00) & (stored_len[1:0] == 2'b00))
               last_transfer_o_len = stored_len[9:2] - 8'h1;
            else if((address_i[1:0] == 2'b10 && stored_len[1:0] == 2'b11) || (address_i[1:0] == 2'b11 && stored_len[1:0] >= 2'b10))
               last_transfer_o_len = stored_len[9:2] + 8'h1;
            else last_transfer_o_len = stored_len[9:2];
         end
      end  // if(AXI_DATA_W == 32)
      if (AXI_DATA_W == 64) begin
         assign symbolsToRead_o            = (|length_i[2:0]) ? (length_i >> 3) + 1 : length_i >> 3;
         assign max_transfer_len           = min(MAX_LENGTH * 8,13'h0800);
         assign max_transfer_len_minus_one = min((MAX_LENGTH * 8) - 8,13'h07F8);
         wire [2:0] address_i_plus_len = address_i[2:0] + stored_len[2:0];
         always @* begin
            last_transfer_o_len = 0;

            if ((address_i[2:0] == 3'b000) && (stored_len[2:0] == 3'b000))
               last_transfer_o_len = stored_len[10:3] - 8'h1;
            else if (!(address_i_plus_len >= address_i[2:0] || address_i_plus_len == 0))
               last_transfer_o_len = stored_len[10:3] + 8'h1;
            else last_transfer_o_len = stored_len[10:3];
         end
      end  // if(AXI_DATA_W == 64)
      if (AXI_DATA_W == 128) begin
         assign symbolsToRead_o            = (|length_i[3:0]) ? (length_i >> 4) + 1 : length_i >> 4;
         assign max_transfer_len           = min(MAX_LENGTH * 16,13'h1000);
         assign max_transfer_len_minus_one = min((MAX_LENGTH * 16) - 16,13'h0FF0);
         wire [3:0] address_i_plus_len = address_i[3:0] + stored_len[3:0];
         always @* begin
            last_transfer_o_len = 0;

            if ((address_i[3:0] == 4'b0000) && (stored_len[3:0] == 4'b0000))
               last_transfer_o_len = stored_len[11:4] - 8'h1;
            else if (!(address_i_plus_len >= address_i[3:0] || address_i_plus_len == 0))
               last_transfer_o_len = stored_len[11:4] + 8'h1;
            else last_transfer_o_len = stored_len[11:4];

         end
      end  // if(AXI_DATA_W == 128)
      if (AXI_DATA_W == 256) begin
         assign symbolsToRead_o = (|length_i[4:0]) ? (length_i >> 5) + 1 : length_i >> 5;
         assign max_transfer_len           = min(MAX_LENGTH * 32,13'h1000);
         assign max_transfer_len_minus_one = min((MAX_LENGTH * 32) - 32,13'h0FE0); // Because of boundary conditions, cannot go higher
         wire [4:0] address_i_plus_len = address_i[4:0] + stored_len[4:0];
         always @* begin
            last_transfer_o_len = 0;

            if ((address_i[4:0] == 5'h00) && (stored_len[4:0] == 5'h00))
               last_transfer_o_len = stored_len[12:5] - 8'h1;
            else if (!(address_i_plus_len >= address_i[4:0] || address_i_plus_len == 0))
               last_transfer_o_len = stored_len[12:5] + 8'h1;
            else last_transfer_o_len = stored_len[12:5];

         end
      end  // if(AXI_DATA_W == 256)
      if (AXI_DATA_W == 512) begin
         assign symbolsToRead_o = (|length_i[5:0]) ? (length_i >> 6) + 1 : length_i >> 6;
         assign max_transfer_len           = min(MAX_LENGTH * 64,13'h1000);
         assign max_transfer_len_minus_one = min((MAX_LENGTH * 64) - 64,13'h0FC0); // Because of boundary conditions, cannot go higher
         wire [5:0] address_i_plus_len = address_i[5:0] + stored_len[5:0];
         always @* begin
            last_transfer_o_len = 0;

            if ((address_i[5:0] == 6'h00) && (stored_len[5:0] == 6'h00))
               last_transfer_o_len = stored_len[13:6] - 8'h1;
            else if (!(address_i_plus_len >= address_i[5:0] || address_i_plus_len == 0))
               last_transfer_o_len = stored_len[13:6] + 8'h1;
            else last_transfer_o_len = stored_len[13:6];
         end
      end  // if(AXI_DATA_W == 512)
   endgenerate

   // Outside generate, logic is generic enough
   always @(posedge clk_i, posedge rst_i) begin
      if (rst_i) begin
         first_i_transfer  <= 1'b0;
         stored_len        <= 0;
         last_transfer_o   <= 0;
         true_axi_axaddr_o <= 0;
      end else begin
         if (transfer_start_i) begin
            first_i_transfer  <= 1'b1;
            last_transfer_o   <= last_transfer_next_o;
            true_axi_axaddr_o <= true_address_i;
            stored_len        <= length_i;
         end
         if (burst_start_i) begin
            first_i_transfer  <= 1'b0;
            last_transfer_o   <= last_transfer_next_o;
            true_axi_axaddr_o <= true_axi_axaddr_o + ((true_axi_axlen_o + 1) * (AXI_DATA_W / 8));
            stored_len        <= stored_len - transfer_byte_size;
         end
      end
   end

endmodule  // transfer_controller
