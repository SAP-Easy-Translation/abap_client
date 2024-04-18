*&---------------------------------------------------------------------*
*& Report  ZTRANSLATOR_OTHERTEXTS_V2
*& Translation for other texts
*&---------------------------------------------------------------------*
REPORT ztranslator_othertexts_v2 MESSAGE-ID ztranslate_v2.

DATA:
  ##NEEDED
  _obj_name   TYPE lxecollnam,
  ##NEEDED
  _obj_type   TYPE lxeobjtype,
  other_texts TYPE ztrans_longtexts.

SELECTION-SCREEN BEGIN OF BLOCK frame1 WITH FRAME TITLE text-001.
PARAMETERS:
  s_lang  TYPE lxeisolang OBLIGATORY,
  t_lang  TYPE lxeisolang OBLIGATORY,
  custmnr TYPE lxecustmnr OBLIGATORY.
SELECT-OPTIONS:
  obj_ran FOR _obj_name OBLIGATORY MATCHCODE OBJECT devclass.
SELECTION-SCREEN END OF BLOCK frame1.

SELECTION-SCREEN BEGIN OF BLOCK frame2 WITH FRAME.
SELECT-OPTIONS:
  obj_type FOR _obj_type MATCHCODE OBJECT ztrans_text_type.
PARAMETERS:
  layout TYPE slis_vari.

SELECTION-SCREEN END OF BLOCK frame2.

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

  PERFORM:
    translate,
    display_alv_grid.

FORM translate.

  CLEAR other_texts.
  SELECT devclass FROM tdevc
    WHERE devclass IN @obj_ran
    INTO TABLE @DATA(package_names).

  DATA(translator) = NEW ztranslator_v2(
    translator_api_client = NEW ztranslator_v2_dee_api_client( )
    replacement_map = VALUE #( ) ).

  LOOP AT package_names REFERENCE INTO DATA(package_name).
    translator->read_other_texts(
      EXPORTING
        custmnr        = custmnr
        target_lang    = t_lang
        source_lang    = s_lang
        package_name   = CONV lxecollnam( package_name->* )
        obj_type_range = obj_type[]
      CHANGING
        other_texts = other_texts ).
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
      i_callback_user_command  = 'USER_COMMAND'
      i_save                   = 'A'
      is_variant               = variant
      i_structure_name         = 'ZTRANS_LONGTEXT'
      is_layout_lvc            = alv_layout
    TABLES
      t_outtab                 = other_texts.

ENDFORM.

##CALLED
FORM user_command USING command TYPE syst-ucomm
  selfield TYPE slis_selfield.

  CASE command.
    WHEN '&IC1'.
      PERFORM goto_translation USING selfield.
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

FORM goto_translation USING selfield TYPE slis_selfield.
  DATA:
    selected_row_index TYPE i,
    target_lang        TYPE slanguage,
    source_lang        TYPE slanguage,
    master_lang        TYPE slanguage,
    trans_object       TYPE STANDARD TABLE OF lwrkobj.

  PERFORM get_selected_row_index USING selfield
    CHANGING selected_row_index.
  IF selected_row_index IS INITIAL.
    MESSAGE s003.
    RETURN.
  ENDIF.

  DATA(object) = other_texts[ selected_row_index ].

  PERFORM:
    get_lang_key USING s_lang CHANGING source_lang,
    get_lang_key USING t_lang CHANGING target_lang,
    get_lang_key USING object-orig_lang CHANGING master_lang.

  trans_object = VALUE #(
    ( targetlang = target_lang objtype = object-objtype
      objname = object-objname sourcelang = source_lang
      masterlang = master_lang ) ).

  CALL FUNCTION 'SKTU_X_OTYP_TRANSLATION_CALL'
    EXPORTING
      authority_check       = abap_true
      customer              = custmnr
    TABLES
      tlwrkobj              = trans_object
    EXCEPTIONS
      text_get_error        = 4.
  IF sy-subrc = 0.
    RETURN.
  ENDIF.

  trans_object = VALUE #(
    ( targetlang = target_lang objtype = object-objtype
      objname = object-objname sourcelang = master_lang
      masterlang = master_lang ) ).

  CALL FUNCTION 'SKTU_X_OTYP_TRANSLATION_CALL'
    EXPORTING
      authority_check       = abap_true
      customer              = custmnr
    TABLES
      tlwrkobj              = trans_object
    EXCEPTIONS
      text_get_error        = 4.
  IF sy-subrc <> 0.
    MESSAGE s007 DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.

FORM get_lang_key USING iso_locale TYPE lxeisolang
  CHANGING result TYPE spras.
  DATA:
    iso_lang TYPE laiso.

  iso_lang = to_upper( iso_locale+0(2) ).
  SELECT spras FROM t002
    WHERE laiso = @iso_lang
    INTO @result.
  ENDSELECT.

ENDFORM.
