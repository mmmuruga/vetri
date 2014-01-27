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
	parameter C_FAMILY              = "virtex5",
	parameter C_S_AXI_DATA_WIDTH    = 32,          
	parameter C_S_AXI_ADDR_WIDTH    = 32,          
	parameter C_USE_WSTRB           = 0,
	parameter C_DPHASE_TIMEOUT      = 0,
	parameter C_BASEADDR            = 32'hFFFFFFFF,
	parameter C_HIGHADDR            = 32'h00000000,
	parameter C_S_AXI_ACLK_FREQ_HZ  = 100,
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

  // Slave AXI Ports
  input                                     S_AXI_ACLK,
  input                                     S_AXI_ARESETN,
  input      [C_S_AXI_ADDR_WIDTH-1 : 0]     S_AXI_AWADDR, 
  input                                     S_AXI_AWVALID,
  input      [C_S_AXI_DATA_WIDTH-1 : 0]     S_AXI_WDATA,
  input      [C_S_AXI_DATA_WIDTH/8-1 : 0]   S_AXI_WSTRB,
  input                                     S_AXI_WVALID, 
  input                                     S_AXI_BREADY, 
  input      [C_S_AXI_ADDR_WIDTH-1 : 0]     S_AXI_ARADDR, 
  input                                     S_AXI_ARVALID,
  input                                     S_AXI_RREADY, 
  output                                    S_AXI_ARREADY,
  output     [C_S_AXI_DATA_WIDTH-1 : 0]     S_AXI_RDATA,
  output     [1 : 0]                        S_AXI_RRESP,
  output                                    S_AXI_RVALID, 
  output                                    S_AXI_WREADY, 
  output     [1 :0]                         S_AXI_BRESP,
  output                                    S_AXI_BVALID, 
  output                                    S_AXI_AWREADY,

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


reg [31:0] ethertype;
reg [15:0] ETH_TYPE;
reg [31:0] dst_MAC2;
reg [31:0] dst_MAC;
reg search_req;
wire word_IP_DST_HI, word_IP_DST_LO;

   // ------------ Internal Params --------
   localparam HEADER = 8'b00000001;
   localparam DATA   = 8'b00000010;
   localparam FIRST  = 8'b00000100;
   localparam SECOND = 8'b00001000;
   localparam SLEEP  = 8'b00010000;
   localparam LPI    = 8'b00100000;
   localparam WAKE   = 8'b01000000;	
   localparam TBW    = 8'b10000000;
     
   localparam dst_ip_reg_0 = 32'h6501A8C0;
   localparam src_ip_reg_0 = 32'h6401A8C0;
   localparam ip_checksum_reg_0 = 16'b0;
   localparam ip_proto = 8'h00;
   localparam ip_ttl = 8'h40;
   localparam ip_tos = 8'b0;
   localparam src_mac_reg_0 = 48'h112233445566;
   localparam dst_mac_reg_0 = 48'h223344556677;


   //------------- Wires ------------------
   reg [7:0] state, state_next;

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
         .din                           ({s_axis_tlast, s_axis_tuser, s_axis_tstrb, s_axis_tdata}),
         .wr_en                          (s_axis_tvalid & ~in_fifo_nearly_full),
         .rd_en                          (in_fifo_rd_en),
         .reset                          (~axi_resetn),
         .clk                            (axi_aclk));

   // ------------- counters for various states ---------------
reg us_enable;
//reg us_counter_enable;
reg sleep_enable, lpi_enable, wake_enable, inactive;
reg request_counter_enable;

reg [31:0] SLEEP_COUNTER, LPI_COUNTER, WAKE_COUNTER, US_COUNTER;
reg [31:0] request_counter;
reg [31:0] sleep_occurance, lpi_occurance, wake_occurance;

   always @(posedge axi_aclk)
        if(~axi_resetn) begin//{
               SLEEP_COUNTER <='b0;
	       LPI_COUNTER   <='b0;
	       WAKE_COUNTER  <='b0;	 
	       US_COUNTER <='b0;
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
//	else if(us_counter_enable) begin//{
//		US_COUNTER <= US_COUNTER +'b1;
//		end//}

	else	begin//{
		SLEEP_COUNTER <='b0;
                LPI_COUNTER   <='b0;
                WAKE_COUNTER  <='b0;
		US_COUNTER <= US_COUNTER + 'b1; 	
		end//}
		 
always @(posedge axi_aclk)
	if(~axi_resetn) begin//{
	  sleep_occurance ='b0;
          lpi_occurance = 'b0;
          wake_occurance = 'b0;
	end//}

       else if(SLEEP_COUNTER== 'd4) begin//{    //sim  
    //   else if(SLEEP_COUNTER== 'd461) begin//{    //synth
           sleep_occurance = sleep_occurance+'b1;
         end//}
        else if(LPI_COUNTER== 'd10) begin//{     //sim
    //  else if(LPI_COUNTER== 'd6349) begin//{      //synth
           lpi_occurance = lpi_occurance +'b1;
        end//}
        else if(WAKE_COUNTER== 'd7) begin//{     //sim
     //  else if(WAKE_COUNTER== 'd717) begin//{      //synth
            wake_occurance = wake_occurance +'b1;
        end//}


   /******************************************************************
    * Upstream Communication control
    *****************************************************************/ 

wire [31:0] transmission_window;

    always @(posedge axi_aclk)
	if(~axi_resetn) begin//{
		us_enable <= 'b0;
	 end//}
	 //else if(/*(US_COUNTER != transmission_window) &&*/ (ethertype==32'h00000008)) begin//{
	 else if((dst_MAC2==32'h000001fe) && (dst_MAC==32'hcabeba00)) begin//{
		us_enable <= 'b1;
	 end//}
	else begin//{
		us_enable <='b0;
	end//}


   /******************************************************************
    * Get the destination, source and )ethertype of the pkt
    *****************************************************************/
//reg [47:0] dst_MAC;
//reg [15:0] ethertype;
//reg search_req;
//wire word_IP_DST_HI, word_IP_DST_LO;
wire search_req_w;

   preprocess_control
     #(.C_S_AXIS_DATA_WIDTH (C_S_AXIS_DATA_WIDTH)
       ) preprocess_control
       ( // --- Interface to the previous stage
          .tdata                    (s_axis_tdata),
          .valid                    (s_axis_tvalid & ~in_fifo_nearly_full),
          .tlast                    (s_axis_tlast),

         // --- Interface to other preprocess blocks
         .word_IP_DST_HI            (word_IP_DST_HI),
         .word_IP_DST_LO            (word_IP_DST_LO),

         // --- Misc
         .reset                     (~axi_resetn),
         .clk                       (axi_aclk)
         );


   always @(posedge axi_aclk) begin//{
      if(~axi_resetn) begin
         dst_MAC   = 32'b0;
         dst_MAC2  = 32'b0;
         ethertype = 32'b0;
         search_req = 'b0;
      end
      else if(word_IP_DST_HI) begin//{
          //if(search_req_w) begin
            dst_MAC = s_axis_tdata[31:0];
	    dst_MAC2=	{16'b0,s_axis_tdata[47:32]};
            ethertype = {16'b0,s_axis_tdata[111:96]};
            //dst_MAC =  s_axis_tdata[255:208]; // BIG endian
            //ethertype = s_axis_tdata[159:144]; // BIG endian
            search_req = 1;
         end//}
         else begin//{
            search_req     = 0;
         end//}
      end//} // else: !if(reset)

assign search_req_w = (word_IP_DST_HI=='b1)? 'b1:'b0;

// control signal 

reg gate;
wire gate_rw;// checking the logic for gate

always @(posedge axi_aclk) begin//{

if(~axi_resetn) begin//{
	gate <= 'b0;
//	us_counter_enable <= 'b0;
	end//}

// us_counter_enable= 'b1 should be triggered by state machine.

else if(US_COUNTER <= transmission_window) begin//{
                gate <= 'b1;
//		us_counter_enable <= 'b1;
                end//}
else begin//{    
                gate <= 'b0;
//		us_counter_enable <= 'b0;
 end//}

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
/*    TBW  : 
	 	begin //{
		us_counter_enable= 'b1;
			if(US_COUNTER == transmission_window)begin//{
				state_next = SLEEP;
				us_counter_enable = 'b0;
	 			end//}
			else
				state_next = TBW;
                 end//}*/
    FIRST: begin//{
	    inactive=1;
	    //us_counter_enable= 'b1;
            m_axis_tvalid = 'b1;
            m_axis_tdata = {dst_ip_reg_0[15:0],src_ip_reg_0, ip_checksum_reg_0,ip_proto,ip_ttl,48'b0,ip_tos,8'h45,16'h0c88,src_mac_reg_0,dst_mac_reg_0};
            m_axis_tstrb = 'hffffffff;
                m_axis_tuser[DST_PORT_POS+7:DST_PORT_POS] = 8'b100;
                //m_axis_tuser[DST_PORT_POS+7:DST_PORT_POS] = 8'b1;
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
                           state_next = HEADER;
		//	   us_counter_enable= 'b0;  is to be done by the counter.
			   request_counter_enable = 'b1;
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
		  if(SLEEP_COUNTER== 'd4) begin//{            //sim	
//		  if(SLEEP_COUNTER== 'd461) begin//{	      //synth	
                            state_next = LPI;
	       		    sleep_enable='b0;
			end//}
		  else
                        state_next = SLEEP;
		end//}

        LPI:begin//{
		inactive =1;
                lpi_enable='b1;
                 if(LPI_COUNTER== 'd10) begin//{     //sim
//                 if(LPI_COUNTER== 'd6349) begin//{     //synth 
                            state_next = WAKE;
                	    lpi_enable='b0;
                	end//}
		  else
			state_next = LPI;
	     end//}

        WAKE:begin//{
		inactive=1;
                wake_enable='b1;
                 if(WAKE_COUNTER== 'd7) begin//{     //sim
//                 if(WAKE_COUNTER== 'd717) begin//{     //synth
                            state_next = HEADER;	
                	    wake_enable='b0;
                	end//}
		   else
			state_next = WAKE;
		end//}

        //HEADER: if(/*~us_enable*/ gate /*&& (US_COUNTER = transmission_window)*/) begin//{
        HEADER: if(/*(us_enable=='b0)&&*/(gate_rw=='b0)) begin//{
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
                else  begin//{
			  state_next= FIRST;
			end//}

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
      end
      else begin
         state <= state_next;
      end
   end

   // Handle output
   assign in_fifo_rd_en = m_axis_tready && !in_fifo_empty && !inactive;
   assign fm_axis_tvalid = !in_fifo_empty;



  // ---------- Regs part ----------------------------
  wire                                            Bus2IP_Clk;
  wire                                            Bus2IP_Resetn;
  wire     [C_S_AXI_ADDR_WIDTH-1 : 0]             Bus2IP_Addr;
  wire     [0:0]                                  Bus2IP_CS;
  wire                                            Bus2IP_RNW;
  wire     [C_S_AXI_DATA_WIDTH-1 : 0]             Bus2IP_Data;
  wire     [C_S_AXI_DATA_WIDTH/8-1 : 0]           Bus2IP_BE;
  wire     [C_S_AXI_DATA_WIDTH-1 : 0]             IP2Bus_Data;
  wire                                            IP2Bus_RdAck;
  wire                                            IP2Bus_WrAck;
  wire                                            IP2Bus_Error;

localparam NUM_RO_REGS       = 7;
localparam NUM_RW_REGS       = 3;

  wire     [NUM_RW_REGS*C_S_AXI_DATA_WIDTH-1 : 0] rw_regs;
  wire     [NUM_RO_REGS*C_S_AXI_DATA_WIDTH-1 : 0] ro_regs;

  // -- AXILITE IPIF
  axi_lite_ipif_1bar #
  (
   .C_S_AXI_DATA_WIDTH (C_S_AXI_DATA_WIDTH),
   .C_S_AXI_ADDR_WIDTH (C_S_AXI_ADDR_WIDTH),
	.C_USE_WSTRB        (C_USE_WSTRB),
	.C_DPHASE_TIMEOUT   (C_DPHASE_TIMEOUT),
   .C_BAR0_BASEADDR    (C_BASEADDR),
   .C_BAR0_HIGHADDR    (C_HIGHADDR)
  ) axi_lite_ipif_inst
  (
    .S_AXI_ACLK          ( S_AXI_ACLK     ),
    .S_AXI_ARESETN       ( S_AXI_ARESETN  ),
    .S_AXI_AWADDR        ( S_AXI_AWADDR   ),
    .S_AXI_AWVALID       ( S_AXI_AWVALID  ),
    .S_AXI_WDATA         ( S_AXI_WDATA    ),
    .S_AXI_WSTRB         ( S_AXI_WSTRB    ),
    .S_AXI_WVALID        ( S_AXI_WVALID   ),
    .S_AXI_BREADY        ( S_AXI_BREADY   ),
    .S_AXI_ARADDR        ( S_AXI_ARADDR   ),
    .S_AXI_ARVALID       ( S_AXI_ARVALID  ),
    .S_AXI_RREADY        ( S_AXI_RREADY   ),
    .S_AXI_ARREADY       ( S_AXI_ARREADY  ),
    .S_AXI_RDATA         ( S_AXI_RDATA    ),
    .S_AXI_RRESP         ( S_AXI_RRESP    ),
    .S_AXI_RVALID        ( S_AXI_RVALID   ),
    .S_AXI_WREADY        ( S_AXI_WREADY   ),
    .S_AXI_BRESP         ( S_AXI_BRESP    ),
    .S_AXI_BVALID        ( S_AXI_BVALID   ),
    .S_AXI_AWREADY       ( S_AXI_AWREADY  ),
	
	// Controls to the IP/IPIF modules
    .Bus2IP_Clk          ( Bus2IP_Clk     ),
    .Bus2IP_Resetn       ( Bus2IP_Resetn  ),
    .Bus2IP_Addr         ( Bus2IP_Addr    ),
    .Bus2IP_RNW          ( Bus2IP_RNW     ),
    .Bus2IP_BE           ( Bus2IP_BE      ),
    .Bus2IP_CS           ( Bus2IP_CS      ),
    .Bus2IP_Data         ( Bus2IP_Data    ),
    .IP2Bus_Data         ( IP2Bus_Data    ),
    .IP2Bus_WrAck        ( IP2Bus_WrAck   ),
    .IP2Bus_RdAck        ( IP2Bus_RdAck   ),
    .IP2Bus_Error        ( IP2Bus_Error   )
  );
  
  // -- IPIF REGS
  ipif_regs #
  (
    .C_S_AXI_DATA_WIDTH (C_S_AXI_DATA_WIDTH),          
    .C_S_AXI_ADDR_WIDTH (C_S_AXI_ADDR_WIDTH),   
    .NUM_RW_REGS        (NUM_RW_REGS),
    .NUM_RO_REGS        (NUM_RO_REGS)
  ) ipif_regs_inst
  (   
    .Bus2IP_Clk     ( Bus2IP_Clk     ),
    .Bus2IP_Resetn  ( Bus2IP_Resetn  ), 
    .Bus2IP_Addr    ( Bus2IP_Addr    ),
    .Bus2IP_CS      ( Bus2IP_CS[0]   ),
    .Bus2IP_RNW     ( Bus2IP_RNW     ),
    .Bus2IP_Data    ( Bus2IP_Data    ),
    .Bus2IP_BE      ( Bus2IP_BE      ),
    .IP2Bus_Data    ( IP2Bus_Data    ),
    .IP2Bus_RdAck   ( IP2Bus_RdAck   ),
    .IP2Bus_WrAck   ( IP2Bus_WrAck   ),
    .IP2Bus_Error   ( IP2Bus_Error   ),
    .rw_regs        ( rw_regs ),
    .ro_regs        ( ro_regs )
  );
//wire ack;

/**************************************************************
  Register Section
**************************************************************/

wire [31:0] rst_cntrs;
wire enable_signal;

//localparam NUM_RW_REGS       = 5; -- update above
	
assign rst_cntrs = rw_regs[31:0];
//assign us_enable = rw_regs[(C_S_AXI_DATA_WIDTH*1)+1-1:(C_S_AXI_DATA_WIDTH*1)];
assign transmission_window = rw_regs[63:32];
assign gate_rw = rw_regs[95:64];
reg  [C_S_AXI_DATA_WIDTH-1 : 0] stamper;
//remember to change the number of RO REGS
assign ro_regs = {request_counter, US_COUNTER, gate, dst_MAC2, dst_MAC, ethertype, stamper};

// LUT hit/miss counters
  always @ (posedge axi_aclk) begin
    if (~axi_resetn) begin
          stamper  <= {C_S_AXI_DATA_WIDTH{1'b0}};
	  request_counter <= 'b0;
        end
        else if (rst_cntrs) begin
          stamper  <= {C_S_AXI_DATA_WIDTH{1'b0}};
	  request_counter <= 'b0;
        end
	else if (request_counter_enable)
		request_counter <= request_counter +'b1;
        else begin
           stamper  <= stamper + 1;
        end
  end

endmodule // output_port_lookup
