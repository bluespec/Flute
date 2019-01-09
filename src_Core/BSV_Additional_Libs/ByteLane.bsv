// Copyright (c) 2017-2019 Bluespec, Inc.  All Rights Reserved

package ByteLane;

// ================================================================
// Library for 'adjusting/unadjusting' bytes on a wider bus

// Example: a 64b bit bus can be regarded as 8 "byte lanes"
//     with each lane selected by the lower 3 bits of an address.
// Such a bus typically carries data of 1, 2, 4 or 8 bytes.
// For 1,2, and 4, different apps use one of two representations:
// - carry the data on contiguous lower-order bytes, with a separate
//     indication of how many bytes are being carried (1,2,4)
// - carry the data bytes on the lanes corresponding to the byte addresses,
//     along with a 'mask' indicating which bytes are valid:
//        Lane:    7 6 5 4 3 2 1 0
//                 x x x x D D D D    4-byte data, address lsbs = 3'b000
//                 D D D D x x x x    4-byte data, address lsbs = 3'b100
//
//                 x x x x x x D D    2-byte data, address lsbs = 3'b000
//                 x x x x D D x x    2-byte data, address lsbs = 3'b010
//                   ...
//                 D D x x x x x x    2-byte data, address lsbs = 3'b110
//
//                 x x x x x x x D    1-byte data, address lsbs = 3'b000
//                 x x x x x x D x    1-byte data, address lsbs = 3'b001
//                   ...
//                 D x x x x x x x    1-byte data, address lsbs = 3'b111
// Only 1 bit/lane is needed for the 'mask' (a.k.a. 'byte-enable' or 'strobe')

// ================================================================
// BSV library imports

import Vector :: *;

// ================================================================
// Expand each bit in 'strobe' to a byte, creating a full-width mask.

function Bit #(TMul #(n,8)) fn_strobe_to_mask (Bit #(n) strobe);
   function Bit #(8) fn_bit_j_to_byte_j (Integer j);
      return signExtend (strobe [j]);
   endfunction

   Vector #(n, Bit #(8)) v = genWith (fn_bit_j_to_byte_j);
   return pack (v);
endfunction

// ================================================================
// Update an n-byte word taking into account an n-bit strobe

function Bit# (TMul #(n,8)) fn_update_strobed_bytes (Bit# (TMul #(n,8)) old_data,
						     Bit# (TMul #(n,8)) new_data,
						     Bit #(n)           strobe);
   Bit# (TMul #(n,8)) mask = fn_strobe_to_mask (strobe);
   return ((old_data & (~ mask))  |  (new_data & mask));
endfunction

// ================================================================
// Lane-adjust a data word according to the byte-address and data width
// Args: addr:     only bottom few bits are relevant
//       width:    # of relevant bytes: 1, 2, 4, 8
//       data_in:  lsb-justified data
// Results: Bool err:    misaligned, or bad data_width
//          strobe:      bit vector showing which lanes are active
//          data_out:    data shifted to be in-lane

// ----------------
// 32b version

function Tuple3 #(Bool,                // 
		  Bit #(4),            // strobe
		  Bit #(32))           // lane-adjusted data
         fn_lane_adjust_32b (Bit #(32) addr, Bit #(3) dw, Bit #(32) data);

   Bit #(4) strobe = 0;
   Bool     err    = False;

   case (dw)
      1: case (addr [1:0])
	    2'b00: begin strobe = 'b_0001;                      end
	    2'b01: begin strobe = 'b_0010; data = (data << 8);  end
	    2'b10: begin strobe = 'b_0100; data = (data << 16); end
	    2'b11: begin strobe = 'b_1000; data = (data << 24); end
	 endcase
      2: case (addr [1:0])
	    2'b00: begin strobe = 'b_0011;                      end
	    2'b10: begin strobe = 'b_1100; data = (data << 16); end
	    default: err = True;
	 endcase
      4: case (addr [1:0])
	    2'b00: strobe = 'b_1111;
	    default: err = True;
	 endcase
      default: err = True;
   endcase
   return tuple3 (err, strobe, data);
endfunction

// ----------------
// 64b version

function Tuple3 #(Bool,                // err: misaligned, or bad data_width
		  Bit #(8),            // strobe
		  Bit #(64))           // lane-adjusted data
          fn_lane_adjust_64b (Bit #(64) addr, Bit #(4) dw, Bit #(64) data);

   Bit #(8) strobe = 0;
   Bool     err    = False;

   case (dw)
      1: case (addr [2:0])
	    3'b000: begin strobe = 'b_0000_0001;                      end
	    3'b001: begin strobe = 'b_0000_0010; data = (data <<  8); end
	    3'b010: begin strobe = 'b_0000_0100; data = (data << 16); end
	    3'b011: begin strobe = 'b_0000_1000; data = (data << 24); end
	    3'b100: begin strobe = 'b_0001_0000; data = (data << 32); end
	    3'b101: begin strobe = 'b_0010_0000; data = (data << 40); end
	    3'b110: begin strobe = 'b_0100_0000; data = (data << 48); end
	    3'b111: begin strobe = 'b_1000_0000; data = (data << 56); end
	 endcase
      2: case (addr [2:0])
	    3'b000: begin strobe = 'b_0000_0011;                      end
	    3'b010: begin strobe = 'b_0000_1100; data = (data << 16); end
	    3'b100: begin strobe = 'b_0011_0000; data = (data << 32); end
	    3'b110: begin strobe = 'b_1100_0000; data = (data << 48); end
	    default: err = True;
	 endcase
      3: case (addr [2:0])
	    3'b000: begin strobe = 'b_0000_1111;                      end
	    3'b100: begin strobe = 'b_1111_0000; data = (data << 32); end
	    default: err = True;
	 endcase
      4: case (addr [2:0])
	    3'b000: begin strobe = 'b_1111_1111; end
	    default: err = True;
	 endcase
      default: err = True;
   endcase
   return tuple3 (err, strobe, data);
endfunction

// ================================================================
// Lane-unadjust a data word according to the byte-address and data width

// ----------------
// 32b version

function Tuple2 #(Bool,                // err: misaligned, or bad data_width
		  Bit #(32))           // lane-unadjusted data
          fn_lane_unadjust_32b (Bit #(32) addr, Bit #(3) dw, Bit #(32) data);

   Bool     err    = False;

   case (dw)
      1: case (addr [1:0])
	    2'b00: data = (data >>  0);
	    2'b01: data = (data >>  8);
	    2'b10: data = (data >> 16);
	    2'b11: data = (data >> 24);
	 endcase
      2: case (addr [1:0])
	    2'b00: data = (data >>  0);
	    2'b10: data = (data >> 16);
	    default: err = True;
	 endcase
      4: case (addr [1:0])
	    2'b00: data = (data >>  0);
	    default: err = True;
	 endcase
      default: err = True;
   endcase
   return tuple2 (err, data);
endfunction

// ----------------
// 64b version

function Tuple2 #(Bool,                // err: misaligned, or bad data_width
		  Bit #(64))           // lane-unadjusted data
          fn_lane_unadjust_64b (Bit #(64) addr, Bit #(4) dw, Bit #(64) data);

   Bool     err    = False;

   case (dw)
      1: case (addr [2:0])
	    3'b000: data = (data >>  0);
	    3'b001: data = (data >>  8);
	    3'b010: data = (data >> 16);
	    3'b011: data = (data >> 24);
	    3'b100: data = (data >> 32);
	    3'b101: data = (data >> 40);
	    3'b110: data = (data >> 48);
	    3'b111: data = (data >> 56);
	 endcase
      2: case (addr [2:0])
	    3'b000: data = (data >>  0);
	    3'b010: data = (data >> 16);
	    3'b100: data = (data >> 32);
	    3'b110: data = (data >> 48);
	    default: err = True;
	 endcase
      4: case (addr [2:0])
	    3'b000: data = (data >>  0);
	    3'b100: data = (data >> 32);
	    default: err = True;
	 endcase
      8: case (addr [2:0])
	    3'b000: data = (data >>  0);
	    default: err = True;
	 endcase
      default: err = True;
   endcase
   return tuple2 (err, data);
endfunction

// ================================================================

endpackage
