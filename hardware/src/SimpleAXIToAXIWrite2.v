`timescale 1ns / 1ps

module SimpleAXIToAXIWrite2 #(
   parameter AXI_ADDR_W = 32,
   parameter AXI_DATA_W = 32,
   parameter AXI_LEN_W  = 8,
   parameter AXI_ID_W   = 4,
   parameter LEN_W      = 8
) (
   input                             m_wvalid_i,
   output                            m_wready_o,
   input      [      AXI_ADDR_W-1:0] m_waddr_i,
   input      [      AXI_DATA_W-1:0] m_wdata_i,
   input      [(AXI_DATA_W / 8)-1:0] m_wstrb_i,
   input      [           LEN_W-1:0] m_wlen_i,
   output                            m_wlast_o,

   output [AXI_ID_W-1:0] axi_awid_o,
   output [AXI_ADDR_W-1:0] axi_awaddr_o,
   output [AXI_LEN_W-1:0] axi_awlen_o,
   output [3-1:0] axi_awsize_o,
   output [2-1:0] axi_awburst_o,
   output [2-1:0] axi_awlock_o,
   output [4-1:0] axi_awcache_o,
   output [3-1:0] axi_awprot_o,
   output [4-1:0] axi_awqos_o,
   output reg [1-1:0] axi_awvalid_o,
   input [1-1:0] axi_awready_i,
   output [AXI_DATA_W-1:0] axi_wdata_o,
   output reg [(AXI_DATA_W/8)-1:0] axi_wstrb_o,
   output [1-1:0] axi_wlast_o,
   output [1-1:0] axi_wvalid_o,
   input [1-1:0] axi_wready_i,
   input [AXI_ID_W-1:0] axi_bid_i,
   input [2-1:0] axi_bresp_i,
   input [1-1:0] axi_bvalid_i,
   output [1-1:0] axi_bready_o,

   input clk_i,
   input rst_i
);
   
   localparam OFFSET_W = $clog2(AXI_DATA_W / 8);

   // Constant axi values
   assign axi_awid_o    = 0;
   assign axi_awburst_o = 'b01;  // INCR
   assign axi_awlock_o  = 0;
   assign axi_awcache_o = 0;
   assign axi_awprot_o  = 0;
   assign axi_awqos_o   = 0;
   assign axi_bready_o = 1'b1;  // We ignore write response

   wire state_transfer_running;
   reg [OFFSET_W-1:0] reg_stored_offset;
   BurstSplit #(
      .DATA_W(AXI_DATA_W)
   ) split (
      .offset_i(reg_stored_offset),

      .data_in_i   (m_wdata_i),
      .data_valid_i(m_wvalid_i && state_transfer_running),
      .data_ready_o(m_wready_o),

      // Simple interface for data_out
      .data_out_o(axi_wdata_o),
      .data_out_valid_o(axi_wvalid_o),
      .data_out_ready_i(axi_wready_i),

      .clk_i(clk_i),
      .rst_i(rst_i)
   );

   wire [13-1:0] comb_offset;
   wire comb_no_transfer,comb_last_transfer;
   wire [(AXI_DATA_W/8)-1:0] comb_initial_strb,comb_final_strb;
   wire [31-1:0] comb_transfer_symbols;

   reg [AXI_ADDR_W-1:0] reg_address;
   reg [LEN_W-1:0] reg_length;

   AXITransferController2 #(
      .AXI_ADDR_W(AXI_ADDR_W),
      .AXI_DATA_W(AXI_DATA_W),
      .AXI_LEN_W(AXI_LEN_W),
      .LEN_W     (LEN_W)
   ) tempController (
      .in_address(reg_address),
      .in_length(reg_length),

      .out_transfer_offset(comb_offset),
      .out_no_transfer(comb_no_transfer),
      .out_last_transfer(comb_last_transfer),
      .out_initial_strb(comb_initial_strb),
      .out_final_strb(comb_final_strb),
      .out_aligned_transfer_symbols(comb_transfer_symbols),
      .out_axi_axlen(axi_awlen_o),
      .out_axi_axsize(axi_awsize_o)
   );

   reg [AXI_LEN_W-1:0] reg_counter;
   reg [3:0] reg_state;
   always @(posedge clk_i, posedge rst_i) begin
      if (rst_i) begin
         reg_state <= 0;
         reg_stored_offset <= 0;
         reg_address <= 0;
         reg_length <= 0;
         reg_counter <= 0;

         axi_wstrb_o <= 0;
         axi_awvalid_o <= 0;
      end else begin 
         case (reg_state)
            3'h0: begin  // Start of entire transfer. Save values that change
               if (m_wvalid_i) begin
                  reg_stored_offset  <= m_waddr_i[OFFSET_W-1:0];
                  reg_address <= m_waddr_i;
                  reg_length <= m_wlen_i;
                  reg_state      <= 3'h1;
               end
            end
            3'h1: begin  // Start of axi transfer
               axi_awvalid_o <= 1'b1;
               reg_state     <= 3'h2;
            end
            3'h2: begin
               if (axi_awready_i) begin
                  axi_awvalid_o <= 1'b0;
                  reg_state   <= 3'h3;
                  axi_wstrb_o <= comb_initial_strb;
                  reg_counter <= 0;
               end
            end
            3'h3: begin // Transfer is occurring
               if (axi_wvalid_o && axi_wready_i) begin
                  reg_counter <= reg_counter + 1;

                  axi_wstrb_o <= ~0;
                  if ((reg_counter + 1) >= axi_awlen_o) begin
                     axi_wstrb_o <= comb_final_strb;
                  end

                  if (axi_wlast_o) begin
                     if(comb_last_transfer) begin
                        reg_state <= 3'h0;
                     end else begin
                        reg_state <= 3'h1;
                        reg_address <= reg_address + comb_offset;
                        reg_length <= reg_length - comb_offset;
                     end
                  end
               end
            end
         endcase
      end
   end

assign state_transfer_running = (reg_state == 3'h3);
assign m_wlast_o = (state_transfer_running && axi_wvalid_o && axi_wready_i && axi_wlast_o && comb_last_transfer);

assign axi_wlast_o = (reg_counter == axi_awlen_o);
assign axi_awaddr_o = reg_address;

endmodule  // SimpleAXItoAXIWrite
