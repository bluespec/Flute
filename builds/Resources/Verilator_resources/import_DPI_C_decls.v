// Hand-written System Verilog import statements for imported C functions

// ----------------------------------------------------------------
// import "BDPI" function ActionValue #(Bit #(8)) c_trygetchar (Bit #(8) dummy);

   import "DPI-C"
   function  byte unsigned  c_trygetchar (byte unsigned  dummy);

// ----------------------------------------------------------------
