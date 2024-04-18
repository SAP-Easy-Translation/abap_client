*&---------------------------------------------------------------------*
*& Modulpool         ZTRANSLATOR_V2
*&
*&---------------------------------------------------------------------*
PROGRAM ZTRANSLATOR_V2.

MODULE status_0001 OUTPUT.
  SET PF-STATUS 'MAIN'.
ENDMODULE.

MODULE exit_command_0001 INPUT.

  IF sy-ucomm = 'BACK'.
    LEAVE PROGRAM.
  ENDIF.

ENDMODULE.

MODULE user_command_0001 INPUT.

  CASE sy-ucomm.
    WHEN 'SHORT'.
      SUBMIT ztranslator_shorttexts_v2
        AND RETURN.
    WHEN 'LONG'.
      SUBMIT ztranslator_othertexts_v2
        AND RETURN.
  ENDCASE.

ENDMODULE.
