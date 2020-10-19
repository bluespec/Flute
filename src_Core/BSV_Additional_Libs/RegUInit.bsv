package RegUInit;

import "BVI" RegUNInit =
   module mkRegUInit#(parameter a init) (Reg#(a))
      provisos (Bits#(a,sa));

      parameter width = valueOf(sa);
      parameter init  = pack(init);

      default_clock clk(CLK, (*unused*)CLK_GATE);
      default_reset rst(RST);

      method Q_OUT _read();
      method       _write(D_IN) enable(EN);

      schedule _read  CF  _read;
      schedule _write SBR _write;
      schedule _read  SB  _write;
   endmodule
endpackage
