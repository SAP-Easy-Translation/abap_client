CLASS zcx_translate_http_error_v2 DEFINITION
  PUBLIC
  INHERITING FROM zcx_translate_v2
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF http_error,
        msgid TYPE symsgid VALUE 'ZTRANSLATE_V2',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'CODE',
        attr2 TYPE scx_attrname VALUE 'MESSAGE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF http_error .
    CONSTANTS:
      BEGIN OF status_code,
        msgid TYPE symsgid VALUE 'ZTRANSLATE_V2',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'CODE',
        attr2 TYPE scx_attrname VALUE 'MESSAGE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF status_code .
    DATA code TYPE i .
    DATA message TYPE string .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !code     TYPE i OPTIONAL
        !message  TYPE string OPTIONAL .
protected section.
private section.
ENDCLASS.



CLASS ZCX_TRANSLATE_HTTP_ERROR_V2 IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->CODE = CODE .
me->MESSAGE = MESSAGE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
