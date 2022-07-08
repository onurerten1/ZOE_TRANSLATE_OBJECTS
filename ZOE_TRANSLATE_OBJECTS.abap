*&---------------------------------------------------------------------*
*& Report ZOE_TRANSLATE_OBJECTS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zoe_translate_objects.

CLASS lcl_main DEFINITION DEFERRED.

TABLES: tadir.

DATA: go_table TYPE REF TO cl_salv_table,
      go_main  TYPE REF TO lcl_main.

DATA: BEGIN OF gs_data,
        pgmid      LIKE tadir-pgmid,
        object     LIKE tadir-object,
        obj_name   LIKE tadir-obj_name,
        devclass   LIKE tadir-devclass,
        masterlang LIKE tadir-masterlang,
        text       LIKE t100-text,
      END OF gs_data,
      gt_data LIKE TABLE OF gs_data.

DATA: bdcdata LIKE bdcdata OCCURS 0 WITH HEADER LINE.

PARAMETERS: p_pack TYPE tadir-devclass OBLIGATORY,
            p_lanp TYPE spras OBLIGATORY DEFAULT sy-langu,
            p_lant TYPE spras OBLIGATORY.

SELECT-OPTIONS: s_objt FOR tadir-object,
                s_objn FOR tadir-obj_name.

PARAMETERS: p_subp AS CHECKBOX.
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: p_mess AS CHECKBOX.
  SELECTION-SCREEN COMMENT (50) TEXT-001.
SELECTION-SCREEN END OF LINE.


************************************************************************
CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          i_lang_primary   TYPE spras
          i_lang_translate TYPE spras,
      get_data,
      list_data.

    DATA: language_primary   TYPE lxeisolang,
          language_translate TYPE lxeisolang.
ENDCLASS.

CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD get_data.
    DATA: lv_object_name TYPE sobj_name,
          lr_pack        TYPE RANGE OF tadir-devclass,
          lr_pack_temp   TYPE RANGE OF tadir-devclass.

    APPEND VALUE #( sign    = 'I'
                    option  = 'EQ'
                    low     = p_pack ) TO lr_pack.

    IF p_subp = abap_true.
      APPEND VALUE #( sign    = 'I'
                      option  = 'EQ'
                      low     = p_pack ) TO lr_pack_temp.

      DO.
        SELECT devclass
          FROM tdevc
          WHERE parentcl IN @lr_pack_temp
          INTO TABLE @DATA(lt_subpackages).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        CLEAR lr_pack_temp.
        lr_pack_temp = VALUE #( FOR ls_subpackage IN lt_subpackages
                                    sign    = 'I'
                                    option  = 'EQ'
                                    ( low   = ls_subpackage-devclass ) ).
        APPEND LINES OF lr_pack_temp TO lr_pack.
      ENDDO.
    ENDIF.

    SELECT pgmid,
           object,
           obj_name,
           devclass,
           masterlang
      FROM tadir
      WHERE object      IN @s_objt
      AND   obj_name    IN @s_objn
      AND   devclass    IN @lr_pack
      AND   masterlang  = @p_lanp
      AND   delflag     = @abap_false
      ORDER BY devclass,
               object,
               obj_name
      INTO CORRESPONDING FIELDS OF TABLE @gt_data.

    IF p_mess = abap_true.
      DATA(lv_where) = | NOT EXISTS ( SELECT t2~msgnr| &&
                       | FROM t100 AS t2| &&
                       | WHERE t2~sprsl = @p_lant| &&
                       | AND   t2~arbgb = t1~arbgb| &&
                       | AND   t2~msgnr = t1~msgnr )|.
    ENDIF.
    LOOP AT gt_data INTO gs_data WHERE object = 'MSAG'.
      TRY.
        SELECT t1~sprsl,
               t1~arbgb,
               t1~msgnr,
               t1~text
          FROM t100 AS t1
          WHERE t1~sprsl = @p_lanp
          AND   t1~arbgb = @gs_data-obj_name
          AND (lv_where)
          ORDER BY msgnr
          INTO TABLE @DATA(lt_messages).
      CATCH cx_sy_dynamic_osql_syntax.
    ENDTRY.

      LOOP AT lt_messages INTO DATA(ls_messages).
        CLEAR lv_object_name.

        lv_object_name = ls_messages-arbgb.
        lv_object_name+20(3) = ls_messages-msgnr.

        APPEND VALUE #( pgmid       = 'LIMU'
                        object      = 'MESS'
                        obj_name    = lv_object_name
                        devclass    = p_pack
                        masterlang  = ls_messages-sprsl
                        text        = ls_messages-text ) TO gt_data.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD list_data.

    DATA: lo_columns TYPE REF TO cl_salv_columns,
          lo_column  TYPE REF TO cl_salv_column_table.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = go_table
          CHANGING
            t_table      = gt_data ).

        go_table->get_display_settings( )->set_striped_pattern( abap_true ).
        go_table->get_functions( )->set_all( abap_true ).

        lo_columns = go_table->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        go_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        go_table->get_layout( )->set_key( value = VALUE #( report = sy-repid ) ).
        go_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).

        TRY.
            lo_column ?= lo_columns->get_column( 'OBJ_NAME' ).
            lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_data_error.
        ENDTRY.

        DATA(lo_event) = go_table->get_event( ).
        DATA(lo_handler) = NEW lcl_handler( ).
        SET HANDLER lo_handler->on_link_click FOR lo_event.

        go_table->display( ).
      CATCH cx_salv_msg.

    ENDTRY.

  ENDMETHOD.
  METHOD constructor.
    SELECT SINGLE language
      FROM lxe_t002x
      WHERE r3_lang = @i_lang_primary
      AND   language NOT LIKE '%++'
      INTO @language_primary.

    SELECT SINGLE language
      FROM lxe_t002x
      WHERE r3_lang = @i_lang_translate
      AND   language NOT LIKE '%++'
      INTO @language_translate.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_link_click.
    CLEAR: gs_data,
           bdcdata,
           bdcdata[].

    IF column = 'OBJ_NAME'.
      TRY.
          gs_data = gt_data[ row ].
        CATCH cx_sy_itab_line_not_found.
          RETURN.
      ENDTRY.

      PERFORM bdc_dynpro      USING 'SAPMTRAN' '1000'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=T'.
      PERFORM bdc_dynpro      USING 'SAPMTRAN' '2000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'DYNP_2000-SLANG'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=PICK'.
      PERFORM bdc_field       USING 'DYNP_2000-O_REQUEST_PGMID'
                                    gs_data-pgmid.
      PERFORM bdc_field       USING 'DYNP_2000-O_REQUEST_OBJECT'
                                    gs_data-object.
      PERFORM bdc_field       USING 'DYNP_2000-O_REQUEST_OBJ_NAME'
                                    gs_data-obj_name.
      PERFORM bdc_field       USING 'DYNP_2000-SLANG'
                                    go_main->language_primary.
      PERFORM bdc_field       USING 'DYNP_2000-TLANG'
                                    go_main->language_translate.

      CALL TRANSACTION 'SE63' USING bdcdata
                              MODE  'E'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

************************************************************************
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*  INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
*  START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  go_main = NEW #( i_lang_primary = p_lanp
                   i_lang_translate = p_lant ).

  go_main->get_data( ).

*----------------------------------------------------------------------*
*  END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF gt_data IS NOT INITIAL.
    go_main->list_data( ).
  ENDIF.