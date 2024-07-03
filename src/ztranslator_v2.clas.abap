CLASS ztranslator_v2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      _obj_type_range TYPE RANGE OF lxeobjtype.

    METHODS constructor
      IMPORTING
        translator_api_client TYPE REF TO zif_translator_v2
        replacement_map       TYPE zif_translator_v2=>_replacement_map.

    METHODS translate_object
      IMPORTING
        !target_lang TYPE lxeisolang
        !source_lang TYPE lxeisolang
        !object      TYPE lxe_colob
      CHANGING
        proposals    TYPE ztranslation_t
      RAISING
        zcx_translate_v2 .
    METHODS translate_objects
      IMPORTING
        !target_lang    TYPE lxeisolang
        !source_lang    TYPE lxeisolang
        !custmnr        TYPE lxecustmnr
        !package_name   TYPE lxecollnam
        !obj_type_range TYPE _obj_type_range
      CHANGING
        proposals       TYPE ztranslation_t
      RAISING
        zcx_translate_v2 .

    METHODS read_other_texts
      IMPORTING
        !obj_type_range TYPE _obj_type_range
        !target_lang    TYPE lxeisolang
        !source_lang    TYPE lxeisolang
        !custmnr        TYPE lxecustmnr
        !package_name   TYPE lxecollnam
      CHANGING
        other_texts     TYPE ztrans_longtexts.

    METHODS save_translations
      IMPORTING
        source_lang TYPE lxeisolang
        target_lang TYPE lxeisolang
        proposals   TYPE ztranslation_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF _trans_object,
        key        TYPE lxe_colob,
        text_pairs TYPE lxe_tt_pcx_s1,
      END OF _trans_object.
    CONSTANTS:
      color_new TYPE ztranslation_color VALUE 'C400'.
    DATA:
      settings              TYPE ztrans_v2_settin,
      translator_api_client TYPE REF TO zif_translator_v2,
      replacement_map       TYPE zif_translator_v2=>_replacement_map.

    METHODS get_app_comp_text
      IMPORTING
        language      TYPE lxeisolang
        package_name  TYPE lxecollnam
      RETURNING
        VALUE(result) TYPE udtext.

    METHODS find_similar_translations
      IMPORTING
        target_lang TYPE lxeisolang
        source_lang TYPE lxeisolang
        source_text TYPE lxeunitlin
      EXPORTING
        proposals   TYPE zif_translator_v2=>_translations.

    METHODS find_stored_proposals
      IMPORTING
        target_lang      TYPE lxeisolang
        source_lang      TYPE lxeisolang
        source_text      TYPE lxeunitlin
        proposal_manager TYPE REF TO if_lxe_pp_base
      EXPORTING
        proposals        TYPE zif_translator_v2=>_translations
      RAISING
        cx_lxe_pp.

    METHODS query_api
      IMPORTING
        target_lang   TYPE lxeisolang
        source_lang   TYPE lxeisolang
        source_text   TYPE lxeunitlin
        app_comp_text TYPE udtext
      EXPORTING
        proposals     TYPE zif_translator_v2=>_translations
      RAISING
        zcx_translate_v2.

    METHODS get_alternatives
      IMPORTING
        proposals     TYPE zif_translator_v2=>_translations
        source_text   TYPE lxeunitlin
      RETURNING
        VALUE(result) TYPE ztranslation_alternatives.

    METHODS add_to_wb_transport
      IMPORTING
        object_key                TYPE lxe_colob
        target_lang               TYPE lxeisolang
      CHANGING
        wb_transport_entries      TYPE tt_e071
        wb_transport_view_entries TYPE e071k_t.

    METHODS store_in_wb_transport
      IMPORTING
        wb_transport_entries             TYPE tt_e071
        VALUE(wb_transport_view_entries) TYPE e071k_t.

ENDCLASS.



CLASS ZTRANSLATOR_V2 IMPLEMENTATION.


  METHOD add_to_wb_transport.
    DATA:
      new_transport_entries      TYPE tt_e071,
      new_transport_view_entries TYPE e071k_t.

    IF object_key-colltyp <> 'R'.
      RETURN.
    ENDIF.
    SELECT COUNT(*) FROM tdevc
      WHERE devclass = @object_key-collnam AND korrflag = @abap_true.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'LXE_OBJ_CREATE_TRANSPORT_ENTRY'
      EXPORTING
        language = target_lang
        custmnr  = object_key-custmnr
        objtype  = object_key-objtype
        objname  = object_key-objname
        tabkey   = '*'
      TABLES
        ex_e071  = new_transport_entries
        ex_e071k = new_transport_view_entries.

    INSERT LINES OF new_transport_entries INTO TABLE wb_transport_entries.
    INSERT LINES OF new_transport_view_entries INTO TABLE wb_transport_view_entries.

  ENDMETHOD.


  METHOD constructor.

    SELECT SINGLE * FROM ztrans_v2_settin
      INTO CORRESPONDING FIELDS OF @settings.

    me->translator_api_client = translator_api_client.
    me->replacement_map = replacement_map.

  ENDMETHOD.


  METHOD find_similar_translations.
    DATA:
      iso_2lang_key TYPE laiso,
      ##NEEDED
      ttype(1),
      length        TYPE int1.

    CLEAR: proposals.
    IF settings-use_similar = abap_false.
      RETURN.
    ENDIF.

    iso_2lang_key = to_upper( source_lang+0(2) ).
    SELECT spras FROM t002
      WHERE laiso = @iso_2lang_key
      INTO @DATA(source_language_key).
    ENDSELECT.
    iso_2lang_key = to_upper( target_lang+0(2) ).
    SELECT spras FROM t002
      WHERE laiso = @iso_2lang_key
      INTO @DATA(target_language_key).
    ENDSELECT.

    DATA(source_dd_text) = CONV as4text( source_text ).
    DESCRIBE FIELD source_dd_text TYPE ttype LENGTH length IN CHARACTER MODE.
    IF strlen( source_text ) <= length.
      SELECT rollname, as4local, as4vers FROM dd04t
        WHERE ddlanguage = @source_language_key AND ddtext = @source_dd_text
        INTO TABLE @DATA(found_data_elements).
      IF sy-subrc = 0.
        SELECT ddtext FROM dd04t
          FOR ALL ENTRIES IN @found_data_elements
          WHERE rollname = @found_data_elements-rollname
          AND ddlanguage = @target_language_key
          AND as4local = @found_data_elements-as4local
          AND as4vers = @found_data_elements-as4vers
          INTO TABLE @proposals.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(source_rep_text) = CONV reptext( source_text ).
    DESCRIBE FIELD source_rep_text TYPE ttype LENGTH length IN CHARACTER MODE.
    IF strlen( source_text ) <= length.
      SELECT rollname, as4local, as4vers FROM dd04t
        WHERE ddlanguage = @source_language_key AND reptext = @source_rep_text
        INTO TABLE @found_data_elements.
      IF sy-subrc = 0.
        SELECT reptext FROM dd04t
          FOR ALL ENTRIES IN @found_data_elements
          WHERE rollname = @found_data_elements-rollname
        AND ddlanguage = @target_language_key
        AND as4local = @found_data_elements-as4local
        AND as4vers = @found_data_elements-as4vers
        INTO TABLE @proposals.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(source_s_text) = CONV scrtext_s( source_text ).
    DESCRIBE FIELD source_s_text TYPE ttype LENGTH length IN CHARACTER MODE.
    IF strlen( source_text ) <= length.
      SELECT rollname, as4local, as4vers FROM dd04t
        WHERE ddlanguage = @source_language_key AND scrtext_s = @source_s_text
        INTO TABLE @found_data_elements.
      IF sy-subrc = 0.
        SELECT scrtext_s FROM dd04t
          FOR ALL ENTRIES IN @found_data_elements
          WHERE rollname = @found_data_elements-rollname
          AND ddlanguage = @target_language_key
          AND as4local = @found_data_elements-as4local
          AND as4vers = @found_data_elements-as4vers
          INTO TABLE @proposals.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(source_m_text) = CONV scrtext_m( source_text ).
    DESCRIBE FIELD source_m_text TYPE ttype LENGTH length IN CHARACTER MODE.
    IF strlen( source_text ) <= length.
      SELECT rollname, as4local, as4vers FROM dd04t
        WHERE ddlanguage = @source_language_key AND scrtext_m = @source_m_text
        INTO TABLE @found_data_elements.
      IF sy-subrc = 0.
        SELECT scrtext_m FROM dd04t
          FOR ALL ENTRIES IN @found_data_elements
          WHERE rollname = @found_data_elements-rollname
          AND ddlanguage = @target_language_key
          AND as4local = @found_data_elements-as4local
          AND as4vers = @found_data_elements-as4vers
          INTO TABLE @proposals.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(source_l_text) = CONV scrtext_l( source_text ).
    DESCRIBE FIELD source_l_text TYPE ttype LENGTH length IN CHARACTER MODE.
    IF strlen( source_text ) <= length.
      SELECT rollname, as4local, as4vers FROM dd04t
        WHERE ddlanguage = @source_language_key AND scrtext_l = @source_l_text
        INTO TABLE @found_data_elements.
      IF sy-subrc = 0.
        SELECT scrtext_l FROM dd04t
          FOR ALL ENTRIES IN @found_data_elements
          WHERE rollname = @found_data_elements-rollname
          AND ddlanguage = @target_language_key
          AND as4local = @found_data_elements-as4local
          AND as4vers = @found_data_elements-as4vers
          INTO TABLE @proposals.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD find_stored_proposals.

    CLEAR: proposals.
    IF settings-use_proposals = abap_false.
      RETURN.
    ENDIF.

    DATA(stored_proposals) = proposal_manager->get_all_for_source(
      i_src_lang = source_lang i_tgt_lang = target_lang
      i_src_text = source_text ).

    LOOP AT stored_proposals INTO DATA(stored_proposal).

      INSERT CONV string( stored_proposal->get_proposal_text( ) )
        INTO TABLE proposals.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_alternatives.

    result = VALUE #( FOR <p> IN proposals
      ( source_text = source_text target_text = <p> ) ).

  ENDMETHOD.


  METHOD get_app_comp_text.
    DATA:
      iso_2lang_key TYPE laiso.

    iso_2lang_key = language+2(2).
    ##NEEDED
    SELECT spras FROM t002
      WHERE laiso = @iso_2lang_key
      INTO @DATA(language_key).
    ENDSELECT.

    ##NEEDED
    SELECT name FROM tdevc AS p
      INNER JOIN df14t AS t ON t~fctr_id = p~component
      WHERE p~devclass = @package_name
      AND t~langu = @language_key AND addon = @space AND as4local = 'A'
      INTO @result.
    ENDSELECT.

  ENDMETHOD.


  METHOD query_api.

    CLEAR: proposals.
    IF settings-use_api = abap_false.
      RETURN.
    ENDIF.

    translator_api_client->translate(
       EXPORTING
         source_lang = to_upper( source_lang+0(2) )
         target_lang = to_upper( target_lang+0(2) )
         source_text = CONV string( source_text )
         replacement_map = replacement_map
         app_comp_text = app_comp_text
       IMPORTING
         translations = proposals ).

  ENDMETHOD.


  METHOD read_other_texts.
    DATA:
      object_list TYPE lxe_tt_colob,
      state       TYPE lxestattrn.

    SELECT obj_type, typeatt FROM lxe_attob
      WHERE obj_type IN @obj_type_range AND typeatt = 'O'
      INTO TABLE @DATA(object_types).

    LOOP AT object_types REFERENCE INTO DATA(object_type).
      CLEAR object_list.
      CALL FUNCTION 'LXE_OBJ_OBJECTS_GET'
        EXPORTING
          custmnr  = custmnr
          objtype  = CONV lxeobjtype( object_type->* )
          collnam  = package_name
        TABLES
          lt_colob = object_list.

      LOOP AT object_list REFERENCE INTO DATA(object).

        CALL FUNCTION 'LXE_OBJ_TRANSLATION_STATUS2'
          EXPORTING
            t_lang = target_lang
            s_lang = source_lang
            custmnr = custmnr
            objtype = object->*-objtype
            objname = object->*-objname
          IMPORTING
            stattrn = state.
        INSERT VALUE #( objlist = object->*-objlist
          custmnr = object->*-custmnr objtype = object->*-objtype
          objname = object->*-objname orig_lang = object->*-orig_lang
          colltyp = object->*-colltyp collnam = object->*-collnam
          domatyp = object->*-domatyp domanam = object->*-domanam
          status = state
          color = COND #( WHEN state = 'T' THEN space ELSE color_new ) )
          INTO TABLE other_texts.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD save_translations.
    DATA:
      objects                   TYPE HASHED TABLE OF _trans_object
      WITH UNIQUE KEY key,
      text_pairs                TYPE lxe_tt_pcx_s1,
      wb_transport_entries      TYPE tt_e071,
      wb_transport_view_entries TYPE e071k_t.

    LOOP AT proposals REFERENCE INTO DATA(proposal).

      DATA(object_key) = VALUE lxe_colob( custmnr = proposal->*-customer_no
        objtype = proposal->*-objtype objname = proposal->*-objname
        collnam = proposal->*-collname colltyp = proposal->*-colltyp ).
      DATA(text_pair) = CORRESPONDING lxe_pcx_s1( proposal->* ).
      CLEAR text_pairs.
      INSERT text_pair INTO TABLE text_pairs.
      READ TABLE objects ASSIGNING FIELD-SYMBOL(<object>)
        WITH TABLE KEY key = object_key.
      IF sy-subrc = 0.
        INSERT text_pair
          INTO TABLE <object>-text_pairs.
      ELSE.
        INSERT VALUE #( key = object_key
          text_pairs = text_pairs )
          INTO TABLE objects.
      ENDIF.

    ENDLOOP.

    LOOP AT objects REFERENCE INTO DATA(object).

      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
        EXPORTING
          t_lang    = target_lang
          s_lang    = source_lang
          custmnr   = object->*-key-custmnr
          objtype   = object->*-key-objtype
          objname   = object->*-key-objname
          read_only = abap_false
        TABLES
          lt_pcx_s1 = text_pairs.
      LOOP AT object->*-text_pairs INTO text_pair.
        DELETE text_pairs WHERE textkey = text_pair-textkey.
        INSERT text_pair INTO TABLE text_pairs.
      ENDLOOP.

      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
        EXPORTING
          t_lang    = target_lang
          s_lang    = source_lang
          custmnr   = object->*-key-custmnr
          objtype   = object->*-key-objtype
          objname   = object->*-key-objname
        TABLES
          lt_pcx_s1 = object->*-text_pairs.

      add_to_wb_transport(
        EXPORTING
          object_key = object->*-key
          target_lang = target_lang
        CHANGING
          wb_transport_entries = wb_transport_entries
          wb_transport_view_entries = wb_transport_view_entries ).

    ENDLOOP.

    store_in_wb_transport(
      wb_transport_entries = wb_transport_entries
      wb_transport_view_entries = wb_transport_view_entries ).

  ENDMETHOD.


  METHOD store_in_wb_transport.

    IF wb_transport_entries IS INITIAL AND wb_transport_view_entries IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ko200_entries) = CORRESPONDING siw_tab_ko200(
      wb_transport_entries ).

    CALL FUNCTION 'TR_OBJECTS_CHECK'
      TABLES
        wt_ko200                = ko200_entries
        wt_e071k                = wb_transport_view_entries
      EXCEPTIONS
        cancel_edit_other_error = 2
        show_only_other_error   = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_OBJECTS_INSERT'
      TABLES
        wt_ko200 = ko200_entries
        wt_e071k = wb_transport_view_entries.

  ENDMETHOD.


  METHOD translate_object.
    DATA:
      text_pairs TYPE STANDARD TABLE OF lxe_pcx_s1.

    DATA(proposal_manager) = cl_lxe_pp=>factory->getlib_base( ).

    DATA(app_comp_text) = get_app_comp_text(
      language = source_lang
      package_name = COND #( WHEN object-colltyp = 'R' THEN object-collnam ELSE space ) ).

    TRY.
        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
          EXPORTING
            t_lang    = target_lang
            s_lang    = source_lang
            custmnr   = object-custmnr
            objtype   = object-objtype
            objname   = object-objname
            read_only = abap_true
          TABLES
            lt_pcx_s1 = text_pairs.

        LOOP AT text_pairs ASSIGNING FIELD-SYMBOL(<text_pair>).

          IF <text_pair>-t_text IS NOT INITIAL OR <text_pair>-s_text IS INITIAL.
            INSERT VALUE #( textkey = <text_pair>-textkey s_text = <text_pair>-s_text
              t_text = <text_pair>-t_text
              unitmlt = <text_pair>-unitmlt uppcase = <text_pair>-uppcase
              texttype = <text_pair>-texttype customer_no = object-custmnr
              objtype = object-objtype objname = object-objname
              collname = object-collnam colltyp = object-colltyp )
            INTO TABLE proposals.
            CONTINUE.
          ENDIF.

          find_similar_translations(
            EXPORTING
              target_lang = target_lang
              source_lang = source_lang
              source_text = <text_pair>-s_text
            IMPORTING
              proposals = DATA(stored_proposals) ).
          IF stored_proposals IS NOT INITIAL.
            INSERT VALUE #( textkey = <text_pair>-textkey s_text = <text_pair>-s_text
              t_text = stored_proposals[ 1 ] unitmlt = <text_pair>-unitmlt
              uppcase = <text_pair>-uppcase texttype = <text_pair>-texttype
              customer_no = object-custmnr objtype = object-objtype objname = object-objname
              alternatives = get_alternatives( proposals = stored_proposals
                source_text = <text_pair>-s_text )
              new_translation = abap_true color = color_new
              collname = object-collnam colltyp = object-colltyp )
              INTO TABLE proposals.
            CONTINUE.
          ENDIF.

          find_stored_proposals(
            EXPORTING
              target_lang = target_lang
              source_lang = source_lang
              source_text = <text_pair>-s_text
              proposal_manager = proposal_manager
            IMPORTING
              proposals = stored_proposals ).
          IF stored_proposals IS NOT INITIAL.
            INSERT VALUE #( textkey = <text_pair>-textkey s_text = <text_pair>-s_text
              t_text = stored_proposals[ 1 ] unitmlt = <text_pair>-unitmlt
              uppcase = <text_pair>-uppcase texttype = <text_pair>-texttype
              customer_no = object-custmnr objtype = object-objtype objname = object-objname
              alternatives = get_alternatives( proposals = stored_proposals
                source_text = <text_pair>-s_text )
              new_translation = abap_true color = color_new
              collname = object-collnam colltyp = object-colltyp )
              INTO TABLE proposals.
            CONTINUE.
          ENDIF.

          query_api(
            EXPORTING
              target_lang = target_lang
              source_lang = source_lang
              source_text = <text_pair>-s_text
              app_comp_text = app_comp_text
            IMPORTING
              proposals = stored_proposals ).
          IF stored_proposals IS NOT INITIAL.
            INSERT VALUE #( textkey = <text_pair>-textkey s_text = <text_pair>-s_text
              t_text = stored_proposals[ 1 ] unitmlt = <text_pair>-unitmlt
              uppcase = <text_pair>-uppcase texttype = <text_pair>-texttype
              customer_no = object-custmnr objtype = object-objtype objname = object-objname
              alternatives = get_alternatives( proposals = stored_proposals
                source_text = <text_pair>-s_text )
              new_translation = abap_true color = color_new
              collname = object-collnam colltyp = object-colltyp )
              INTO TABLE proposals.
            CONTINUE.
          ENDIF.

          INSERT VALUE #( textkey = <text_pair>-textkey s_text = <text_pair>-s_text
            unitmlt = <text_pair>-unitmlt uppcase = <text_pair>-uppcase
            texttype = <text_pair>-texttype customer_no = object-custmnr
            objtype = object-objtype objname = object-objname
            new_translation = abap_true color = color_new
            collname = object-collnam colltyp = object-colltyp )
            INTO TABLE proposals.

        ENDLOOP.

      CATCH cx_lxe_pp INTO DATA(proposal_error).
        RAISE EXCEPTION TYPE zcx_translate_v2
          EXPORTING
            textid   = zcx_translate_v2=>proposal_loading_failed
            previous = proposal_error.
    ENDTRY.

  ENDMETHOD.


  METHOD translate_objects.
    DATA:
      object_list TYPE STANDARD TABLE OF lxe_colob.

    SELECT obj_type, typeatt FROM lxe_attob
      WHERE obj_type IN @obj_type_range AND typeatt = 'S'
      INTO TABLE @DATA(object_types).

    LOOP AT object_types REFERENCE INTO DATA(object_type).
      CLEAR object_list.
      CALL FUNCTION 'LXE_OBJ_OBJECTS_GET'
        EXPORTING
          custmnr  = custmnr
          objtype  = CONV lxeobjtype( object_type->* )
          colltyp  = 'R'
          collnam  = package_name
          likename = '%'
        TABLES
          lt_colob = object_list.

      LOOP AT object_list REFERENCE INTO DATA(object).

        translate_object(
          EXPORTING
            target_lang = target_lang
            source_lang = source_lang
            object      = object->*
          CHANGING
            proposals = proposals ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
