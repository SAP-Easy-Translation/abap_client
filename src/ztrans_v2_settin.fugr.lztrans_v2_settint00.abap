*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTRANS_V2_SETTIN................................*
DATA:  BEGIN OF STATUS_ZTRANS_V2_SETTIN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRANS_V2_SETTIN              .
*.........table declarations:.................................*
TABLES: *ZTRANS_V2_SETTIN              .
TABLES: ZTRANS_V2_SETTIN               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
