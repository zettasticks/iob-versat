`timescale 1ns / 1ps

module SimpleAXItoAXIWrite #(
   parameter AXI_ADDR_W = 32,
   parameter AXI_DATA_W = 32,
   parameter AXI_LEN_W  = 8,
   parameter AXI_ID_W   = 4,
   parameter LEN_W      = 8
) (
   input                             m_wvalid_i,
   output reg                        m_wready_o,
   input      [      AXI_ADDR_W-1:0] m_waddr_i,
   input      [      AXI_DATA_W-1:0] m_wdata_i,
   input      [(AXI_DATA_W / 8)-1:0] m_wstrb_i,
   input      [           LEN_W-1:0] m_wlen_i,
   output reg                        m_wlast_o,

   output [AXI_ID_W-1:0] axi_awid_o,
   output [AXI_ADDR_W-1:0] axi_awaddr_o,
   output [AXI_LEN_W-1:0] axi_awlen_o,
   output [3-1:0] axi_awsize_o,
   output [2-1:0] axi_awburst_o,
   output [2-1:0] axi_awlock_o,
   output [4-1:0] axi_awcache_o,
   output [3-1:0] axi_awprot_o,
   output [4-1:0] axi_awqos_o,
   output [1-1:0] axi_awvalid_o,
   input [1-1:0] axi_awready_i,
   output [AXI_DATA_W-1:0] axi_wdata_o,
   output [(AXI_DATA_W/8)-1:0] axi_wstrb_o,
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

   localparam [2:0] axi_size = ((AXI_DATA_W == 16)   ? 3'b001 : 
                                (AXI_DATA_W == 32)   ? 3'b010 :
                                (AXI_DATA_W == 64)   ? 3'b011 :
                                (AXI_DATA_W == 128)  ? 3'b100 :
                                (AXI_DATA_W == 256)  ? 3'b101 :
                                (AXI_DATA_W == 512)  ? 3'b110 : 
                                (AXI_DATA_W == 1024) ? 3'b111 : 3'b000);

   reg                                         [         2:0] state;
   wire                                                       burst_ready_in;

   reg                                                        flush_burst_split;
   wire data_valid = m_wvalid_i && m_wready_o;

   reg                                         [OFFSET_W-1:0] stored_offset;

   BurstSplit #(
      .DATA_W(AXI_DATA_W)
   ) split (
      .offset_i(stored_offset),

      .data_in_i   (m_wdata_i),
      .data_valid_i(data_valid),

      // Simple interface for data_out
      .data_out_o(axi_wdata_o),

      .clk_i(clk_i),
      .rst_i(rst_i)
   );

   wire        write_last_transfer;
   wire        write_last_transfer_next;

   wire [AXI_LEN_W-1:0] true_axi_awlen;
   reg [AXI_LEN_W-1:0] write_axi_len;
   assign axi_awlen_o = write_axi_len;

   wire [31:0] symbolsToRead_next;
   reg  [31:0] symbolsToRead;

   reg [(AXI_DATA_W/8)-1:0] initial_strb, final_strb;

   AXITransferController #(
      .AXI_ADDR_W(AXI_ADDR_W),
      .AXI_DATA_W(AXI_DATA_W),
      .AXI_LEN_W(AXI_LEN_W),
      .LEN_W     (LEN_W)
   ) write_controller (
      .address_i(m_waddr_i),
      .length_i (m_wlen_i),   // In bytes

      .transfer_start_i(state == 3'h0 && m_wvalid_i),
      .burst_start_i   (state == 3'h2 && axi_awready_i && axi_awvalid_o),

      .initial_strb_o(initial_strb),
      .final_strb_o  (final_strb),

      .symbolsToRead_o(symbolsToRead_next),

      .true_axi_axaddr_o(axi_awaddr_o),

      // TODO: Register these signals to 
      .true_axi_axlen_o    (true_axi_awlen),
      .last_transfer_o     (write_last_transfer),
      .last_transfer_next_o(write_last_transfer_next),

      .clk_i(clk_i),
      .rst_i(rst_i)
   );

   // Address write constants
   assign axi_awid_o    = 'b0;
   assign axi_awsize_o  = axi_size;
   assign axi_awburst_o = 'b01;  // INCR
   assign axi_awlock_o  = 'b0;
   assign axi_awcache_o = 'h2;
   assign axi_awprot_o  = 'b010;
   assign axi_awqos_o   = 'h0;

   reg [(AXI_DATA_W/8)-1:0] wstrb;
   //assign axi_wdata_o = m_wdata_i;
   assign axi_wstrb_o  = wstrb;

   assign axi_bready_o = 1'b1;  // We ignore write response

   reg awvalid, wvalid;
   assign axi_awvalid_o = awvalid;
   assign axi_wvalid_o  = wvalid;

   reg m_axi_last;
   assign axi_wlast_o = m_axi_last;

   reg        first_transfer;

   reg [31:0] symbolsRead;

   // This contains both the logic for the AXI transfer and the databus transfer.
   // We first need to decouple this before progressing.
   reg [AXI_LEN_W-1:0] counter;
   reg [31:0] full_counter;
   always @(posedge clk_i, posedge rst_i) begin
      if (rst_i) begin
         state          <= 0;
         awvalid        <= 0;
         counter        <= 0;
         full_counter   <= 0;
         write_axi_len  <= 0;
         symbolsToRead  <= 0;
         symbolsRead    <= 0;
         stored_offset  <= 0;
         first_transfer <= 0;
         wstrb          <= 0;
      end else begin
         if(m_wvalid_i && m_wready_o) begin
            symbolsRead <= symbolsRead + 1;
         end

         case (state)
            3'h0: begin  // Wait one cycle for transfer controller to calculate things.
               if (m_wvalid_i) begin
                  //awvalid <= 1'b1;
                  stored_offset  <= m_waddr_i[OFFSET_W-1:0];
                  state          <= 3'h1;
                  first_transfer <= 1'b1;
                  symbolsRead <= 0;
               end
            end
            3'h1: begin  // Save values that change 
               symbolsToRead <= symbolsToRead_next;
               write_axi_len <= true_axi_awlen;
               awvalid       <= 1'b1;
               state         <= 3'h2;
            end
            3'h2: begin  // Write address set
               if (axi_awready_i) begin
                  awvalid <= 1'b0;
                  state   <= 3'h4;
                  if (first_transfer) begin
                     first_transfer <= 1'b0;
                     wstrb          <= initial_strb;
                  end else begin
                     wstrb <= ~0;
                     if ((write_axi_len == 0) && write_last_transfer_next) begin
                        wstrb <= final_strb;
                     end
                  end
               end
            end
            3'h4: begin
               if (axi_wvalid_o && axi_wready_i) begin
                  counter <= counter + 1;

                  if (((counter + 1) == axi_awlen_o) && write_last_transfer) begin
                     wstrb <= final_strb;
                  end else begin
                     wstrb <= ~0;
                  end

                  full_counter <= full_counter + 1;
                  if (m_axi_last) begin
                     counter <= 0;
                     if (write_last_transfer) begin
                        full_counter  <= 0;
                        symbolsToRead <= 0;
                        write_axi_len <= 0;
                        state         <= 3'h0;
                     end else begin
                        state <= 3'h1;
                     end
                  end
               end
            end
         endcase
      end
   end

   // TODO: Works but it is not good. Need to simplify this part as well as the Read equivalent. 
   assign m_wready_o = (state != 0 && symbolsRead < symbolsToRead && axi_wready_i);
   assign m_wlast_o = (state != 0 && symbolsRead + 1 >= symbolsToRead);

   always @* begin
      flush_burst_split = 1'b0;
      m_axi_last        = 1'b0;
      wvalid            = 1'b0;

      if (full_counter == symbolsToRead) begin
         flush_burst_split = 1'b1;
      end

      if (state == 3'h3) begin
         wvalid     = 1'b1;
      end

      if (state == 3'h4) begin
         wvalid     = m_wvalid_i || flush_burst_split;

         if (counter == axi_awlen_o) m_axi_last = 1'b1;
      end
   end

endmodule  // SimpleAXItoAXIWrite
