/*
 * Generated by Bluespec Compiler, version 2021.07-22-g61dc0ebb (build 61dc0ebb)
 * 
 * To automatically register this VPI wrapper with a Verilog simulator use:
 *     #include "vpi_wrapper_rand32.h"
 *     void (*vlog_startup_routines[])() = { rand32_vpi_register, 0u };
 * 
 * For a Verilog simulator which requires a .tab file, use the following entry:
 * $imported_rand32 call=rand32_calltf size=32 acc=rw:%TASK
 * 
 * For a Verilog simulator which requires a .sft file, use the following entry:
 * $imported_rand32 vpiSysFuncSized 32 unsigned
 */
#include <stdlib.h>
#include <vpi_user.h>
#include "bdpi.h"

/* the type of the wrapped function */
unsigned int rand32();

/* VPI wrapper function */
PLI_INT32 rand32_calltf(PLI_BYTE8 *user_data)
{
  vpiHandle hCall;
  unsigned int vpi_result;
  vpiHandle *handle_array;
  
  /* retrieve handle array */
  hCall = vpi_handle(vpiSysTfCall, 0);
  handle_array = vpi_get_userdata(hCall);
  if (handle_array == NULL)
  {
    vpiHandle hArgList;
    hArgList = vpi_iterate(vpiArgument, hCall);
    handle_array = malloc(sizeof(vpiHandle) * 1u);
    handle_array[0u] = hCall;
    vpi_put_userdata(hCall, handle_array);
    vpi_free_object(hArgList);
  }
  
  /* create return value */
  make_vpi_result(handle_array[0u], &vpi_result, DIRECT);
  
  /* call the imported C function */
  vpi_result = rand32();
  
  /* copy out return value */
  put_vpi_result(handle_array[0u], &vpi_result, DIRECT);
  vpi_free_object(hCall);
  
  return 0;
}

/* sft: $imported_rand32 vpiSysFuncSized 32 unsigned */

/* tab: $imported_rand32 call=rand32_calltf size=32 acc=rw:%TASK */

PLI_INT32 rand32_sizetf(PLI_BYTE8 *user_data)
{
  return 32u;
}

/* VPI wrapper registration function */
void rand32_vpi_register()
{
  s_vpi_systf_data tf_data;
  
  /* Fill in registration data */
  tf_data.type = vpiSysFunc;
  tf_data.sysfunctype = vpiSizedFunc;
  tf_data.tfname = "$imported_rand32";
  tf_data.calltf = rand32_calltf;
  tf_data.compiletf = 0u;
  tf_data.sizetf = rand32_sizetf;
  tf_data.user_data = "$imported_rand32";
  
  /* Register the function with VPI */
  vpi_register_systf(&tf_data);
}
