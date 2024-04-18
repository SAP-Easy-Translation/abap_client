INTERFACE zif_translator_v2
  PUBLIC .


  TYPES:
    BEGIN OF _replacement,
      original TYPE xstring,
      replaced TYPE xstring,
    END OF _replacement .
  TYPES:
    _replacement_map TYPE STANDARD TABLE OF _replacement,
    _translations    TYPE STANDARD TABLE OF string
    WITH NON-UNIQUE KEY table_line.

  METHODS translate
    IMPORTING
      !source_lang     TYPE laiso
      !target_lang     TYPE laiso
      !source_text     TYPE string
      !replacement_map TYPE _replacement_map
      !app_comp_text   TYPE udtext
    EXPORTING
      translations     TYPE _translations
    RAISING
      zcx_translate_v2 .
ENDINTERFACE.
