/*******************************************************************************
 *
 *  NetFPGA-10G http://www.netfpga.org
 *
 *  File:
 *        nf10_nic_output_port_lookup.v
 *
 *  Library:
 *        hw/std/pcores/nf10_nic_output_port_lookup_v1_00_a
 *
 *  Module:
 *        nf10_nic_output_port_lookup
 *
 *  Author:
 *        Adam Covington
 *
 *  Description:
 *        Hardwire the hardware interfaces to CPU and vice versa
 *
 *  Copyright notice:
 *        Copyright (C) 2010, 2011 The Board of Trustees of The Leland Stanford
 *                                 Junior University
 *
 *  Licence:
 *        This file is part of the NetFPGA 10G development base package.
 *
 *        This file is free code: you can redistribute it and/or modify it under
 *        the terms of the GNU Lesser General Public License version 2.1 as
 *        published by the Free Software Foundation.
 *
 *        This package is distributed in the hope that it will be useful, but
 *        WITHOUT ANY WARRANTY; without even the implied warranty of
 *        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *        Lesser General Public License for more details.
 *
 *        You should have received a copy of the GNU Lesser General Public
 *        License along with the NetFPGA source package.  If not, see
 *        http://www.gnu.org/licenses/.
 *
 */

module nf10_nic_output_port_lookup
#(
    //Master AXI Stream Data Width
    parameter C_M_AXIS_DATA_WIDTH=256,
    parameter C_S_AXIS_DATA_WIDTH=256,
    parameter C_M_AXIS_TUSER_WIDTH=128,
    parameter C_S_AXIS_TUSER_WIDTH=128,
    parameter SRC_PORT_POS=16,
    parameter DST_PORT_POS=24
)
(
    // Global Ports
    input axi_aclk,
    input axi_resetn,

    // Master Stream Ports (interface to data path)
    output reg [C_M_AXIS_DATA_WIDTH - 1:0] m_axis_tdata,
    output reg [((C_M_AXIS_DATA_WIDTH / 8)) - 1:0] m_axis_tstrb,
    output reg [C_M_AXIS_TUSER_WIDTH-1:0] m_axis_tuser,
    output reg m_axis_tvalid,
    input  m_axis_tready,
    output reg m_axis_tlast,

    // Slave Stream Ports (interface to RX queues)
    input [C_S_AXIS_DATA_WIDTH - 1:0] s_axis_tdata,
    input [((C_S_AXIS_DATA_WIDTH / 8)) - 1:0] s_axis_tstrb,
    input [C_S_AXIS_TUSER_WIDTH-1:0] s_axis_tuser,
    input  s_axis_tvalid,
    output s_axis_tready,
    input  s_axis_tlast
);

   function integer log2;
      input integer number;
      begin
         log2=0;
         while(2**log2<number) begin
            log2=log2+1;
         end
      end
   endfunction // log2

// ---------------extra regs declaration --
    wire [C_M_AXIS_DATA_WIDTH - 1:0] fm_axis_tdata;
    wire [((C_M_AXIS_DATA_WIDTH / 8)) - 1:0] fm_axis_tstrb;
    wire [C_M_AXIS_TUSER_WIDTH-1:0] fm_axis_tuser;
    wire fm_axis_tlast;
    wire fm_axis_tvalid;

   // ------------ Internal Params --------
   localparam HEADER = 7'b0000001;
   localparam DATA   = 7'b0000010;
   localparam FIRST  = 7'b0000100;
   localparam SECOND = 7'b0001000;
   localparam SLEEP  = 7'b0010000;
   localparam LPI    = 7'b0100000;
   localparam WAKE   = 7'b1000000;	

   localparam dst_ip_reg_0 = 32'h6501A8C0;
   localparam src_ip_reg_0 = 32'h6401A8C0;
   localparam ip_checksum_reg_0 = 16'b0;
   localparam ip_proto = 8'h00;
   localparam ip_ttl = 8'h40;
   localparam ip_tos = 8'b0;
   localparam src_mac_reg_0 = 48'h112233445566;
   localparam dst_mac_reg_0 = 48'h223344556677;



   //------------- Wires ------------------
   reg [6:0] state, state_next;

   // ------------ Modules ----------------

   fallthrough_small_fifo
        #( .WIDTH(C_M_AXIS_DATA_WIDTH+C_M_AXIS_TUSER_WIDTH+C_M_AXIS_DATA_WIDTH/8+1),
           .MAX_DEPTH_BITS(2))
      input_fifo
        (// Outputs
         .dout                           ({fm_axis_tlast,fm_axis_tuser, fm_axis_tstrb,fm_axis_tdata}),
         .full                           (),
         .nearly_full                    (in_fifo_nearly_full),
         .prog_full                      (),
         .empty                          (in_fifo_empty),
         // Inputs
         .din                            ({s_axis_tlast, s_axis_tuser, s_axis_tstrb, s_axis_tdata}),
         .wr_en                          (s_axis_tvalid & ~in_fifo_nearly_full),
         .rd_en                          (in_fifo_rd_en),
         .reset                          (~axi_resetn),
         .clk                            (axi_aclk));

  // ---------------for ack generation ----

   wire ack;
   reg [2:0] packets;
   assign packet_enable = m_axis_tlast & m_axis_tvalid & m_axis_tready;
   always @(posedge axi_aclk)
        if(~axi_resetn)
                packets<='b0;
        else if(packet_enable)
                packets<= packets+'b1;
        else
                packets<=packets;
   assign ack = (packets == 'd5)? 'b1:'b0;

  // --------------- buffer_empty generation ---

   wire buffer_empty;
   reg  in_fifo_empty_d, in_fifo_empty_dd;
   always @(posedge axi_aclk)
        if(~axi_resetn) begin//{
               in_fifo_empty_d <='b0;
	       in_fifo_empty_dd <='b0;
		end//}
        else begin//{
               in_fifo_empty_d <= in_fifo_empty;
               in_fifo_empty_dd <= in_fifo_empty_d;
	     end//}

   assign buffer_empty = in_fifo_empty_dd & ~in_fifo_empty_d;

 // --------------- timer circuits -----
   reg [31:0] buffer_empty_timer;
   reg buffer_empty_d, buffer_empty_dd, buffer_empty_d3, buffer_empty_d4, buffer_empty_d5, buffer_empty_d6, buffer_dummy;
   reg buffer_empty_d7, buffer_empty_d8, buffer_empty_d9, buffer_empty_d10, buffer_empty_d11, buffer_empty_d12, buffer_empty_d13, buffer_empty_d14, buffer_empty_d15;
   wire buffer_empty_signal;

   always @(posedge axi_aclk)
        if(~axi_resetn) begin//{
               buffer_empty_d <='b0;
               buffer_empty_dd <='b0;
	       buffer_empty_d3 <='b0;
	       buffer_empty_d4 <='b0;
	       buffer_empty_d5 <='b0;
	       buffer_empty_d6 <='b0;
               buffer_empty_d7 <= 'b0;
               buffer_empty_d8 <= 'b0;
               buffer_empty_d9 <= 'b0;
               buffer_empty_d10 <= 'b0;
               buffer_empty_d11 <= 'b0;
               buffer_empty_d12 <= 'b0;
               buffer_empty_d13 <= 'b0;
               buffer_empty_d14 <= 'b0;
               buffer_empty_d15 <= 'b0;
                end//}
        else begin//{
               buffer_empty_d <=  in_fifo_empty;
               buffer_empty_dd <= buffer_empty_d;
               buffer_empty_d3 <= buffer_empty_dd;
               buffer_empty_d4 <= buffer_empty_d3;
               buffer_empty_d5 <= buffer_empty_d4;
               buffer_empty_d6 <= buffer_empty_d5;
               buffer_empty_d7 <= buffer_empty_d6;
               buffer_empty_d8 <= buffer_empty_d7;
               buffer_empty_d9 <= buffer_empty_d8;
               buffer_empty_d10 <= buffer_empty_d9;
               buffer_empty_d11 <= buffer_empty_d10;
               buffer_empty_d12 <= buffer_empty_d11;
               buffer_empty_d13 <= buffer_empty_d12;
               buffer_empty_d14 <= buffer_empty_d13;
               buffer_empty_d15 <= buffer_empty_d14;
             end//}
//  assign buffer_empty_signal = (~buffer_empty_dd) | (~buffer_empty_d) | (~buffer_empty_d3) | (~buffer_empty_d4) | (~buffer_empty_d5) | buffer_empty_d6 | buffer_dummy;

assign buffer_empty_signal = (buffer_empty_dd)& (buffer_empty_d)& (buffer_empty_d3)& (buffer_empty_d4)&(buffer_empty_d5)& buffer_empty_d6 & (buffer_empty_d7)& (buffer_empty_d8)& (buffer_empty_d9)& (buffer_empty_d10)&(buffer_empty_d11)& buffer_empty_d12 & buffer_empty_d13; 
   always @(posedge axi_aclk)
        if(~axi_resetn) begin//{
               buffer_empty_timer <='b0;
                end//}
        else if(buffer_empty_signal =='b1) begin//{
             	buffer_empty_timer <= buffer_empty_timer +'b1;
		end//}

   // ------------- counters for various states ---------------

reg sleep_enable, lpi_enable, wake_enable, inactive;
reg [31:0] SLEEP_COUNTER, LPI_COUNTER, WAKE_COUNTER;

   always @(posedge axi_aclk)
        if(~axi_resetn) begin//{
               SLEEP_COUNTER <='b0;
	       LPI_COUNTER   <='b0;
	       WAKE_COUNTER  <='b0;	 
                end//}
        else if(sleep_enable) begin//{
               SLEEP_COUNTER <= SLEEP_COUNTER + 'b1;
		end//}
	else if(lpi_enable) begin//{
		LPI_COUNTER  <= LPI_COUNTER + 'b1;
		end//}
	else if(wake_enable) begin//{
		WAKE_COUNTER <= WAKE_COUNTER +'b1;
		end//}
	else	begin//{
		SLEEP_COUNTER <='b0;
                LPI_COUNTER   <='b0;
                WAKE_COUNTER  <='b0; 	
		end//}
		

   // ------------- Logic ----------------

   assign s_axis_tready = !in_fifo_nearly_full;

   // modify the dst port in tuser
   always @(*) begin//{
      state_next      = state;
      m_axis_tvalid = fm_axis_tvalid;
      m_axis_tdata = fm_axis_tdata;
      m_axis_tstrb = fm_axis_tstrb;
      m_axis_tuser = fm_axis_tuser;
      m_axis_tlast = fm_axis_tlast;
      sleep_enable = 'b0;
      lpi_enable='b0;
      wake_enable='b0;
      inactive = 'b0;
      case(state)//{
	FIRST: begin//{
		inactive=1;
		m_axis_tvalid = 'b1;
		m_axis_tdata = {dst_ip_reg_0[15:0],src_ip_reg_0, ip_checksum_reg_0,ip_proto,ip_ttl,48'b0,ip_tos,8'h45,16'h0c88,src_mac_reg_0,dst_mac_reg_0};
		m_axis_tstrb = 'hffffffff;
                m_axis_tuser[DST_PORT_POS+7:DST_PORT_POS] = 8'b100;
                m_axis_tlast = 'b0;
	      	if(m_axis_tready) begin//{
			    state_next = SECOND;
			end//} 
		end//}

	SECOND: begin//{
		inactive=1;
		m_axis_tvalid = 'b1;
		m_axis_tdata = {240'hdeadbeefbabe ,dst_ip_reg_0[31:16]};	
		m_axis_tstrb = 'hffffffff;
                m_axis_tuser[DST_PORT_POS+7:DST_PORT_POS] = 8'b100;
		m_axis_tlast = 1; 
	   	if(m_axis_tlast & m_axis_tvalid & m_axis_tready) begin//{
	   		   state_next = SLEEP;
	  		 end//}
		end//}

// we actually want to use clock having a frequency of 156.25 MHz, but the clock used in the design is..
// axi_aclk= 160 MHz -> time = 6.25ns 
// According to 803.az standard 
// Ts  = 2.88 us, this translates to 2.88/6.25 *1000 cycles  = 460.8  cycles = 461 appx
// Tlpi= 4.48 us                     4.48/6.25 *1000 cycles  = 716.8  cycles = 717 appx
// Tw  = 39.68 us	  	     39.68/6.25 *1000 cycles = 6348.8 cycles = 6349 appx
	 
	SLEEP:begin//{
		inactive =1;
		sleep_enable='b1;
		  if(SLEEP_COUNTER== 'd3) begin//{	
                            state_next = LPI;
	       		    sleep_enable='b0;
			end//}
		  else
                        state_next = SLEEP;
                
		end//}

        LPI:begin//{
		inactive =1;
                lpi_enable='b1;
                  if(LPI_COUNTER== 'd6) begin//{     
                            state_next = WAKE;
                	    lpi_enable='b0;
                	end//}
		  else
			state_next = LPI;
	     end//}

        WAKE:begin//{
		inactive=1;
                wake_enable='b1;
                  if(WAKE_COUNTER== 'd4) begin//{     
                            state_next = HEADER;
                	    wake_enable='b0;
                	end//}
		   else
			state_next = WAKE;
		end//}

	HEADER: if(~buffer_empty_signal) begin//{
			inactive = 0;
			m_axis_tvalid = fm_axis_tvalid;
		        m_axis_tdata = fm_axis_tdata;
                        m_axis_tstrb = fm_axis_tstrb;
                        m_axis_tuser = fm_axis_tuser;
                        m_axis_tlast = fm_axis_tlast;	
              		if(m_axis_tvalid & m_axis_tready) begin//{
                            state_next = DATA;
                        end//}
		 end//}
		else state_next = FIRST;

        DATA: begin//{
		   inactive=0;
	           if(m_axis_tlast & m_axis_tvalid & m_axis_tready) begin//{
        	      state_next = HEADER;
          		 end//}
        	end//}
	
	default: state_next = HEADER;

      endcase //} case (state)
   end //} always @ (*)

   always @(posedge axi_aclk) begin
      if(~axi_resetn) begin
	 state <= HEADER;
	 //sleep_enable <= 'b0;
      end
      else begin
	 state <= state_next;
      end
   end

   // Handle output
   assign in_fifo_rd_en = m_axis_tready && !in_fifo_empty && !inactive;
   assign fm_axis_tvalid = !in_fifo_empty;

endmodule // output_port_lookup
