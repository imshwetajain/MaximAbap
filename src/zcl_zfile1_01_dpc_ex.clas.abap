class ZCL_ZFILE1_01_DPC_EX definition
  public
  inheriting from ZCL_ZFILE1_01_DPC
  create public .

public section.
protected section.

  methods FILE1SET_CREATE_ENTITY
    redefinition .
  methods FILE1SET_GET_ENTITYSET
    redefinition .
  methods FILE1SET_GET_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZFILE1_01_DPC_EX IMPLEMENTATION.


  METHOD FILE1SET_CREATE_ENTITY.
    io_data_provider->read_entry_data(
     IMPORTING
     es_data = er_entity ).

    DATA: lw_file TYPE zfile.

    lw_file-filename = er_entity-filename.
    lw_file-value    = er_entity-value.
    lw_file-mimetype = er_entity-mimetype.
    lw_file-sydate  = sy-datum.
    lw_file-sytime  = sy-uzeit.

    INSERT INTO zfile VALUES lw_file.
  ENDMETHOD.


  method FILE1SET_GET_ENTITY.
**TRY.
*CALL METHOD SUPER->FILE1SET_GET_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_request_object       =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**  IMPORTING
**    er_entity               =
**    es_response_context     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA: lv_filename type char30,
          ls_upld    type zfile.
read table it_key_tab assigning FIELD-SYMBOL(<fs_key>) index 1.
lv_filename = <fs_key>-value.
  SELECT SINGLE * FROM zfile
    into ls_upld
  WHERE filename = lv_filename .

MOVE-CORRESPONDING ls_upld to er_entity.

  endmethod.


  METHOD FILE1SET_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->FILE1SET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    SELECT
        mandt
        filename
*    SYDATE
*    SYTIME
        value
        mimetype
      FROM zfile
      INTO CORRESPONDING FIELDS OF TABLE et_entityset.

  ENDMETHOD.
ENDCLASS.
