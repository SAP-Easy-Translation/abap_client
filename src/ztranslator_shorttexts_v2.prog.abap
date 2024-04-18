*&---------------------------------------------------------------------*
*& Report  ZTRANSLATOR_V2
*& Translates Texts from ABAP Objects (Dynpros, GUI-Texts etc.)
*&---------------------------------------------------------------------*
REPORT ztranslator_shorttexts_v2 MESSAGE-ID ztranslate_v2.

TABLES: ztrans_v2_settin, sscrfields.

CONSTANTS:
  BEGIN OF answers,
    yes(1) TYPE c VALUE '1',
    no(1)  TYPE c VALUE '2',
  END OF answers.

DATA:
  ##NEEDED
  _obj_name         TYPE lxecollnam,
  ##NEEDED
  _obj_type         TYPE lxeobjtype,
  proposals         TYPE ztranslation_t,
  translator        TYPE REF TO ztranslator_v2,
  proposals_changed TYPE sap_bool.

CLASS data_changed_listener DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS on_data_changed FOR EVENT
        data_changed OF cl_gui_alv_grid.

ENDCLASS.

CLASS data_changed_listener IMPLEMENTATION.

  METHOD on_data_changed.

    proposals_changed = abap_true.

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK frame1 WITH FRAME TITLE text-001.
PARAMETERS:
  log_file TYPE filename-fileintern,
  s_lang   TYPE lxeisolang OBLIGATORY,
  t_lang   TYPE lxeisolang OBLIGATORY,
  custmnr  TYPE lxecustmnr OBLIGATORY.
SELECT-OPTIONS:
  obj_ran FOR _obj_name OBLIGATORY MATCHCODE OBJECT devclass.
SELECTION-SCREEN END OF BLOCK frame1.

SELECTION-SCREEN BEGIN OF BLOCK frame2 WITH FRAME.
SELECT-OPTIONS:
  obj_type FOR _obj_type MATCHCODE OBJECT ztrans_text_type.
PARAMETERS:
  layout TYPE slis_vari.

SELECTION-SCREEN END OF BLOCK frame2.

SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.
  sscrfields-functxt_01 = text-001.

AT SELECTION-SCREEN.

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM read_settings.
      CALL SCREEN '0001'.
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR layout.
  DATA(variant) = VALUE disvariant( report = sy-repid
    variant = layout ).
  CALL FUNCTION 'LVC_VARIANT_SAVE_LOAD'
    EXPORTING
      i_save_load = 'F'
      i_tabname   = '1'
    CHANGING
      cs_variant  = variant
    EXCEPTIONS
      OTHERS      = 1.
  IF sy-subrc = 0.
    layout = variant-variant.
  ENDIF.

START-OF-SELECTION.
  DATA:
    replacement_map TYPE zif_translator_v2=>_replacement_map.

  IF log_file IS NOT INITIAL.
    ztranslator_replacement_v2=>parse_from_file(
      EXPORTING
        logical_filename = log_file
      IMPORTING
        replacement_map = replacement_map ).
  ENDIF.

  translator = NEW ztranslator_v2(
    translator_api_client = NEW ztranslator_v2_dee_api_client( )
    replacement_map = replacement_map ).
  TRY.
      PERFORM:
        translate,
        display_alv_grid.
    CATCH cx_static_check INTO DATA(fault).
      MESSAGE fault TYPE 'E'.
  ENDTRY.

FORM translate
  RAISING zcx_translate_v2.

  CLEAR proposals.
  SELECT devclass FROM tdevc
    WHERE devclass IN @obj_ran
    INTO TABLE @DATA(package_names).

  LOOP AT package_names REFERENCE INTO DATA(package_name).
    translator->translate_objects(
      EXPORTING
        target_lang    = t_lang
        source_lang    = s_lang
        custmnr        = custmnr
        package_name   = CONV lxecollnam( package_name->* )
        obj_type_range = obj_type[]
      CHANGING
        proposals = proposals ).
  ENDLOOP.

ENDFORM.

FORM display_alv_grid.
  DATA:
    fieldcatalog TYPE lvc_t_fcat.

  DATA(variant) = VALUE disvariant(
    report = sy-repid variant = layout ).

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZTRANSLATION'
    CHANGING
      ct_fieldcat      = fieldcatalog.

  fieldcatalog[ fieldname = 'T_TEXT' ]-edit = abap_true.
  DELETE fieldcatalog WHERE fieldname = 'COLOR'.
  DATA(alv_layout) = VALUE lvc_s_layo( info_fname = 'COLOR' ).

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = variant-report
      i_callback_pf_status_set = 'GUI_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      i_save                   = 'A'
      is_variant               = variant
      it_fieldcat_lvc          = fieldcatalog
      is_layout_lvc            = alv_layout
    TABLES
      t_outtab                 = proposals.

ENDFORM.

FORM add_data_changed_listener.
  DATA:
    grid TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = grid.

  SET HANDLER data_changed_listener=>on_data_changed
    FOR grid.

ENDFORM.

##CALLED
FORM gui_status USING exclude TYPE slis_t_extab.
  SET PF-STATUS 'GRID' EXCLUDING exclude.
  PERFORM add_data_changed_listener.
ENDFORM.

##CALLED
FORM user_command USING command TYPE syst-ucomm
  selfield TYPE slis_selfield.

  CASE command.
    WHEN 'LBACK' OR 'LEXIT'.
      PERFORM ask_for_save_changes.
    WHEN 'PROPOSALS'.
      PERFORM show_proposals USING selfield.
    WHEN 'SAVE'.
      PERFORM save_translations.
  ENDCASE.

ENDFORM.

FORM read_settings.

  SELECT SINGLE * FROM ztrans_v2_settin.

ENDFORM.

FORM ask_for_save_changes.
  DATA:
    answer(1) TYPE c,
    grid      TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = grid.
  grid->check_changed_data( ).

  IF proposals_changed = abap_false.
    SET SCREEN 0.
    LEAVE SCREEN.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question = text-002
    IMPORTING
      answer        = answer.

  CASE answer.
    WHEN answers-yes.
      PERFORM save_translations.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN answers-no.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDFORM.

FORM get_selected_row_index USING selfield TYPE slis_selfield
  CHANGING result TYPE i.
  DATA:
    grid                 TYPE REF TO cl_gui_alv_grid.

  IF selfield-tabindex IS NOT INITIAL.
    result = selfield-tabindex.
  ENDIF.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = grid.

  grid->get_selected_rows(
    IMPORTING
      et_index_rows = DATA(selected_row_index) ).
  IF selected_row_index IS NOT INITIAL.
    result = selected_row_index[ 1 ]-index.
  ENDIF.

ENDFORM.

FORM show_proposals USING selfield TYPE slis_selfield.
  DATA:
    selected_row_index   TYPE i,
    selected_alternative TYPE STANDARD TABLE OF ddshretval.

  PERFORM get_selected_row_index USING selfield
    CHANGING selected_row_index.

  IF selected_row_index IS INITIAL.
    MESSAGE s003.
    RETURN.
  ENDIF.

  DATA(alternatives) = proposals[ selected_row_index ]-alternatives.
  IF lines( alternatives ) < 1.
    MESSAGE s004.
    RETURN.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TARGET_TEXT'
      value_org  = 'S'
    TABLES
      value_tab  = alternatives
      return_tab = selected_alternative.

  IF selected_alternative IS INITIAL.
    RETURN.
  ENDIF.

  proposals[ selected_row_index ]-t_text = selected_alternative[ 1 ]-fieldval.

ENDFORM.

FORM save_translations.
  DATA:
    grid                 TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = grid.
  grid->check_changed_data( ).

  translator->save_translations(
    proposals = proposals
    source_lang = s_lang
    target_lang = t_lang ).
  proposals_changed = abap_false.
  MESSAGE s005.

ENDFORM.

FORM save_settings.

  MODIFY ztrans_v2_settin.
  MESSAGE s006.

ENDFORM.

FORM user_command_0001.

  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'SAVE'.
      PERFORM save_settings.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDFORM.

MODULE status_0001 OUTPUT.
  SET PF-STATUS 'SETTINGS'.
ENDMODULE.

MODULE user_command_0001 INPUT.
  PERFORM user_command_0001.
ENDMODULE.
