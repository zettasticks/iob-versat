`timescale 1ns / 1ps

// Fully combinatorial module that implements AXI transfer logic. 
// Logic allows usercode to implement multiple transfers, handles initial, final strobes and 4k boundary.
// Usercode is responsible for keeping state and updating it.
module AXITransferController2 #(
   parameter AXI_ADDR_W   = 32,
   parameter AXI_DATA_W   = 32,
   parameter AXI_LEN_W    = 8,
   parameter LEN_W        = 32,

   // Do not change these
   parameter MAX_AXI_TRANSFER_W = 13 // Because of 4k boundary condition (plus 1 bit). Any AXI_ADDR_W smaller than this does not need to worry about multiple transfers and as such does not need to care about any offset wire.
) (
   input             [AXI_ADDR_W-1:0] in_address, // In byte space
   input                  [LEN_W-1:0] in_length,  // In bytes

   output reg    [(AXI_DATA_W/8)-1:0] out_initial_strb,
   output reg    [(AXI_DATA_W/8)-1:0] out_final_strb,

   output [MAX_AXI_TRANSFER_W-1:0] out_transfer_offset, // In bytes. Multiple transfers can be perform by incrementing address and decrementing length by this value 

   output                          out_no_transfer, // Set if no transfer. (Length equal to zero).
   output                          out_last_transfer,

   output [MAX_AXI_TRANSFER_W-1:0] out_aligned_transfer_symbols, 

   output reg         [AXI_LEN_W-1:0] out_axi_axlen, // Already decremented by 1 to align with axi requirements.

   output                       [2:0] out_axi_axsize
   );

   // Computed params
   localparam OFFSET_W = $clog2((AXI_DATA_W / 8));
   localparam STROBE_W = AXI_DATA_W / 8;
   localparam MAX_LENGTH = (2 ** AXI_LEN_W);

   function [2:0] calculate_AXI_AXSIZE(input [31:0] axi_data_w);
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


   wire [AXI_DATA_W-1:0] OFFSET_MASK = (0 | {OFFSET_W{1'b1}});

   function [LEN_W-1:0] LEN_min(input [LEN_W-1:0] a, b);
      begin
         LEN_min = (a < b) ? a : b;
      end
   endfunction

   function [MAX_AXI_TRANSFER_W-1:0] MAX_AXI_TRANSFER_min(input [MAX_AXI_TRANSFER_W-1:0] a, b);
      begin
         MAX_AXI_TRANSFER_min = (a < b) ? a : b;
      end
   endfunction

   // Constants that depend on parameters
   wire [MAX_AXI_TRANSFER_W-1:0] const_max_transfer_len;
   wire [MAX_AXI_TRANSFER_W-1:0] const_max_transfer_len_minus_one;

   generate
      if (AXI_DATA_W == 32) begin
         assign const_max_transfer_len           = MAX_AXI_TRANSFER_min(MAX_LENGTH * 4,13'h0400);
         assign const_max_transfer_len_minus_one = MAX_AXI_TRANSFER_min(const_max_transfer_len - 4,13'h03FC);
      end
      if (AXI_DATA_W == 64) begin
         assign const_max_transfer_len           = MAX_AXI_TRANSFER_min(MAX_LENGTH * 8,13'h0800);
         assign const_max_transfer_len_minus_one = MAX_AXI_TRANSFER_min(const_max_transfer_len - 8,13'h07F8);
      end
      if (AXI_DATA_W == 128) begin
         assign const_max_transfer_len           = MAX_AXI_TRANSFER_min(MAX_LENGTH * 16,13'h1000);
         assign const_max_transfer_len_minus_one = MAX_AXI_TRANSFER_min(const_max_transfer_len - 16,13'h0FF0);
      end
      if (AXI_DATA_W == 256) begin
         assign const_max_transfer_len           = 13'h1000;
         assign const_max_transfer_len_minus_one = MAX_AXI_TRANSFER_min(const_max_transfer_len - 32,13'h0FE0); // Because of boundary conditions, cannot go higher
      end
      if (AXI_DATA_W == 512) begin
         assign const_max_transfer_len           = 13'h1000;
         assign const_max_transfer_len_minus_one = MAX_AXI_TRANSFER_min(const_max_transfer_len - 64,13'h0FC0); // Because of boundary conditions, cannot go higher
      end
   endgenerate
   
   wire [MAX_AXI_TRANSFER_W-1:0] ext_extended_address; // Left most bit always 0.
   generate
      if(MAX_AXI_TRANSFER_W > AXI_ADDR_W) begin
         assign ext_extended_address = {{(MAX_AXI_TRANSFER_W - AXI_ADDR_W){1'b0}},in_address[AXI_ADDR_W-1:0]};
      end else if(AXI_ADDR_W >= MAX_AXI_TRANSFER_W) begin
         assign ext_extended_address = {1'b0,in_address[MAX_AXI_TRANSFER_W-2:0]};
      end
   endgenerate

   wire [MAX_AXI_TRANSFER_W-1:0] ext_length; // Saturated left most bit.
   generate
      if(MAX_AXI_TRANSFER_W > LEN_W) begin
         assign ext_length = {{(MAX_AXI_TRANSFER_W - LEN_W){1'b0}},in_length[LEN_W-1:0]};
      end else if(LEN_W >= MAX_AXI_TRANSFER_W) begin
         assign ext_length = {|in_length[LEN_W-1:MAX_AXI_TRANSFER_W-1],in_length[MAX_AXI_TRANSFER_W-2:0]};
      end
   endgenerate

   reg [$clog2(STROBE_W):0] comb_initial_strb_count;

   wire [OFFSET_W-1:0] to_align_amount = STROBE_W - in_address[OFFSET_W-1:0];
   wire [OFFSET_W-1:0] misalign_amount = in_address[OFFSET_W-1:0];

   wire [MAX_AXI_TRANSFER_W-1:0] comb_size_to_next_boundary = (13'h1000 - ext_extended_address);

   reg is_next_boundary_limited;
   reg is_max_transfer_len_limited;
   wire comb_is_transfer_misaligned = (in_address[OFFSET_W-1:0] != 0);

   reg [MAX_AXI_TRANSFER_W-1:0] comb_transfer_len;
   always @* begin
      is_next_boundary_limited = 1'b0;
      is_max_transfer_len_limited = 1'b0;

      if(ext_length >= comb_size_to_next_boundary && (comb_size_to_next_boundary < const_max_transfer_len)) begin
         is_next_boundary_limited = 1'b1;
      end
      if(ext_length >= const_max_transfer_len && (const_max_transfer_len < comb_size_to_next_boundary)) begin
         is_max_transfer_len_limited = 1'b1;
      end

      comb_transfer_len = 0;

      if(is_max_transfer_len_limited) begin
         if(comb_is_transfer_misaligned) begin
            // We 
            comb_transfer_len = const_max_transfer_len - (STROBE_W - comb_initial_strb_count);
         end else begin
            comb_transfer_len = const_max_transfer_len;
         end
      end

      if(is_next_boundary_limited) begin
         comb_transfer_len = comb_size_to_next_boundary;
      end

      if(!is_next_boundary_limited && !is_max_transfer_len_limited) begin
         comb_transfer_len = ext_length;
      end
   end

   // Strobes
   // For the cases where we only transfer a few bytes (need to generate strobes like 0110 and so on)
   // This value does not make sense for any multicycle transfer since 

   // First transfer strobe
   integer i;
   integer ii;
   always @* begin
      out_initial_strb = {STROBE_W{1'b0}};
      comb_initial_strb_count = 0;

      for (i = 0; i < STROBE_W; i = i + 1) begin
         if (i < in_length[OFFSET_W-1:0]) out_initial_strb[i] = 1'b1;
      end      

      out_initial_strb = out_initial_strb << in_address[OFFSET_W-1:0];

      if (in_length >= ((1 << OFFSET_W) - in_address[OFFSET_W-1:0])) begin
         out_initial_strb = (~0) << in_address[OFFSET_W-1:0];
      end

      for (ii = 0; ii < STROBE_W; ii = ii + 1) begin
         comb_initial_strb_count = comb_initial_strb_count + out_initial_strb[ii];
      end
   end

   assign out_transfer_offset = comb_transfer_len;
   assign out_aligned_transfer_symbols = ((out_transfer_offset - 1) >> OFFSET_W) + 1;
   assign out_last_transfer = (out_transfer_offset == in_length);
   assign out_axi_axsize = calculate_AXI_AXSIZE(AXI_DATA_W);
   assign out_no_transfer = (|in_length == 0);

   reg [LEN_W-1:0] temp_out_axi_axlen;

   always @* begin
      temp_out_axi_axlen = 0;
      if(comb_is_transfer_misaligned) begin
         temp_out_axi_axlen = (out_transfer_offset + misalign_amount + STROBE_W - 1) >> OFFSET_W;
      end else begin
         temp_out_axi_axlen = (out_transfer_offset + STROBE_W - 1) >> OFFSET_W;
      end

      if(in_length == 0) begin
         temp_out_axi_axlen = 0;
      end

      if(temp_out_axi_axlen >= 9'h100) begin
         out_axi_axlen = 8'hff;
      end else begin
         out_axi_axlen = temp_out_axi_axlen - 1;
      end

      if(in_length == 0) begin
         out_axi_axlen = 0;
      end
   end

   // final strobe is the remains of the first strobe data + all the full strobe transfers.
   reg [MAX_AXI_TRANSFER_W-1:0] offset_without_initial_strobe;
   reg [$clog2(STROBE_W)-1:0] offset_remaining;

   integer k;
   always @* begin
      offset_without_initial_strobe = out_transfer_offset - comb_initial_strb_count;
      // Removing initial strobe bytes transfered and all the full transfers, the only thing remaining must be the final strobe
      offset_remaining = offset_without_initial_strobe[$clog2(STROBE_W)-1:0];

      out_final_strb = {STROBE_W{1'b0}};
      for(k = 0; k < STROBE_W; k = k + 1) begin
         if(k < offset_remaining) begin
            out_final_strb[k] = 1'b1;
         end
      end
      if(offset_remaining == 0) begin
         out_final_strb = {STROBE_W{1'b1}};         
      end
   end

endmodule
