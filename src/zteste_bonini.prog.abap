*&---------------------------------------------------------------------*
*& Report ZTESTE_BONINI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zteste_bonini.

  data: lv_char20 TYPE string,
        lv_num(3) TYPE n.

  lv_char20 = '10.001'.

  data(lv_string) = VALUE string( ).

  FIND FIRST OCCURRENCE OF REGEX '^\d{0,13}(\.\d{0,2}$|$)' IN lv_char20.

  FIND REGEX '^\d{0,13}\.\d{0,2}$' IN lv_char20 IGNORING CASE.
*                   SUBMATCHES sub1 sub2 sub3 sub4 sub5 sub6.

  IF sy-subrc = 0.

  ENDIF.

  lv_string = '1 <> 0'.

  DATA: lv_ok TYPE xfeld.

  TRY .




*    lv_ok = COND #( LET lv_ret = abap_true
*                    IN  ).

*    IF ( lv_string ).
*
*      BREAK-POINT.
*
*    ENDIF.


  CATCH cx_root INTO data(lv_root).

  ENDTRY.

  data: lt_ekpo TYPE TABLE OF ekpo.

  SELECT ebeln, ebelp, menge
*         SUM( CASE shkzg
*                 WHEN 'H' THEN -1 * menge
*                 WHEN 'D' THEN menge
*               END ) AS SOMA
    FROM rseg
               INTO TABLE @data(lt_rseg).
*               GROUP BY ebeln, ebelp.

  data(lv_sum) = REDUCE menge_d( INIT x = 0 FOR wa IN lt_rseg
                                 WHERE ( ebeln = 'VALOR DO LOOP' AND ebelp = 'VALOR DO LOOP' )
                                 NEXT x = x + wa-menge ).

  BREAK-POINT.
