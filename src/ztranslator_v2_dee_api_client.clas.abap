CLASS ztranslator_v2_dee_api_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_translator_v2 .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF _deepl_translation,
        text                     TYPE string,
        detected_source_language TYPE string,
      END OF _deepl_translation.

    DATA:
      settings TYPE ztrans_v2_settin,
      base_url TYPE string.

    METHODS process_response
      IMPORTING
        response            TYPE REF TO if_http_entity
        replacement_map     TYPE zif_translator_v2=>_replacement_map
      RETURNING
        VALUE(translations) TYPE zif_translator_v2=>_translations.

    METHODS json_to_upper_case
      IMPORTING
        response        TYPE REF TO if_http_entity
        replacement_map TYPE zif_translator_v2=>_replacement_map
      RETURNING
        VALUE(result)   TYPE xstring.

ENDCLASS.



CLASS ZTRANSLATOR_V2_DEE_API_CLIENT IMPLEMENTATION.


  METHOD constructor.

    SELECT SINGLE * FROM ztrans_v2_settin
      INTO CORRESPONDING FIELDS OF @settings.
    base_url = COND #( WHEN settings-use_pro_version = abap_true
      THEN 'https://api.deepl.com/v2/translate'
      ELSE 'https://api-free.deepl.com/v2/translate' ).

  ENDMETHOD.


  METHOD json_to_upper_case.

    DATA(raw_response_body) = response->get_data( ).
    LOOP AT replacement_map REFERENCE INTO DATA(replacement).
      REPLACE ALL OCCURRENCES OF replacement->*-original
        IN raw_response_body WITH replacement->*-replaced IN BYTE MODE.
    ENDLOOP.

    DATA(reader) = cl_sxml_string_reader=>create( raw_response_body ).
    DATA(writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
    DO.
      DATA(node) = reader->read_next_node( ).
      IF node IS INITIAL.
        EXIT.
      ENDIF.
      IF node->type = if_sxml_node=>co_nt_element_open.
        DATA(attributes)  = CAST if_sxml_open_element( node )->get_attributes( ).
        LOOP AT attributes ASSIGNING FIELD-SYMBOL(<attribute>).
          IF <attribute>->qname-name = 'name'.
            <attribute>->set_value(
              to_upper( <attribute>->get_value( ) ) ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      writer->write_node( node ).
    ENDDO.
    result = CAST cl_sxml_string_writer( writer )->get_output( ).

  ENDMETHOD.


  METHOD process_response.
    DATA:
      deepl_translations TYPE STANDARD TABLE OF _deepl_translation.

    DATA(json_structure) = json_to_upper_case( response = response
      replacement_map = replacement_map ).

    CALL TRANSFORMATION id
      SOURCE XML json_structure
      RESULT translations = deepl_translations.

    translations = VALUE #(
     FOR <dt> IN deepl_translations ( <dt>-text ) ).

  ENDMETHOD.


  METHOD zif_translator_v2~translate.

    cl_http_client=>create_by_url(
      EXPORTING
        url = base_url
      IMPORTING
        client = DATA(http_client) ).

    http_client->request->set_method( if_http_request=>co_request_method_post ).
    http_client->request->set_header_field( name = 'Authorization'
      value = |DeepL-Auth-Key { settings-api_key }| ).
    http_client->request->set_form_field( name = 'text'
      value = source_text ).
    http_client->request->set_form_field( name = 'source_lang'
      value = CONV string( source_lang ) ).
    http_client->request->set_form_field( name = 'target_lang'
      value = CONV string( target_lang ) ).
    http_client->request->set_form_field( name = 'context'
      value = CONV string( app_comp_text ) ).
    http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      http_client->get_last_error(
        IMPORTING
          code    = DATA(error_code)
          message = DATA(error_message) ).
      http_client->close( ).
      RAISE EXCEPTION TYPE zcx_translate_http_error_v2
        EXPORTING
          textid  = zcx_translate_http_error_v2=>http_error
          code    = error_code
          message = error_message.
    ENDIF.
    http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      http_client->get_last_error(
        IMPORTING
          code    = error_code
          message = error_message ).
      http_client->close( ).
      RAISE EXCEPTION TYPE zcx_translate_http_error_v2
        EXPORTING
          textid  = zcx_translate_http_error_v2=>http_error
          code    = error_code
          message = error_message.
    ENDIF.

    http_client->response->get_status(
      IMPORTING
        code = DATA(http_status_code)
        reason = DATA(reason) ).
    IF http_status_code NOT BETWEEN 200 AND 299.
      http_client->close( ).
      RAISE EXCEPTION TYPE zcx_translate_http_error_v2
        EXPORTING
          textid  = zcx_translate_http_error_v2=>status_code
          code    = error_code
          message = reason.
    ENDIF.

    translations = process_response( response = CAST if_http_entity( http_client->response )
      replacement_map = replacement_map ).
    http_client->close( ).

  ENDMETHOD.
ENDCLASS.
