CLASS ztranslator_replacement_v2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS parse_from_file
      IMPORTING
        !logical_filename TYPE filename-fileintern
      EXPORTING
        !replacement_map  TYPE zif_translator_v2=>_replacement_map .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS json_to_upper_case
      IMPORTING
        json_content  TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring.

ENDCLASS.



CLASS ZTRANSLATOR_REPLACEMENT_V2 IMPLEMENTATION.


  METHOD json_to_upper_case.

    DATA(reader) = cl_sxml_string_reader=>create( json_content ).
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


  METHOD parse_from_file.
    CONSTANTS: empty_byte(1) TYPE x VALUE IS INITIAL.
    DATA:
      file_path      TYPE string,
      line(500)      TYPE x,
      content        LIKE STANDARD TABLE OF line,
      content_length TYPE i,
      json_content   TYPE xstring.

    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
        logical_filename = logical_filename
      IMPORTING
        file_name        = file_path.

    DESCRIBE FIELD line LENGTH DATA(length) IN BYTE MODE.
    OPEN DATASET file_path FOR INPUT IN BINARY MODE.
    DO.
      READ DATASET file_path INTO line MAXIMUM LENGTH length
        ACTUAL LENGTH DATA(act_length).
      IF sy-subrc <> 0 AND line+0(1) = empty_byte.
        EXIT.
      ENDIF.
      INSERT line INTO TABLE content.
      ADD act_length TO content_length.
    ENDDO.
    CLOSE DATASET file_path.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = content_length
      IMPORTING
        buffer       = json_content
      TABLES
        binary_tab   = content.
    json_content = json_to_upper_case( json_content ).

    CALL TRANSFORMATION id
      SOURCE XML json_content
      RESULT replacement = replacement_map.

  ENDMETHOD.
ENDCLASS.
