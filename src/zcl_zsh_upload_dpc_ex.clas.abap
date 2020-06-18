class ZCL_ZSH_UPLOAD_DPC_EX definition
  public
  inheriting from ZCL_ZSH_UPLOAD_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_STREAM
    redefinition .
protected section.

  methods FILESET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSH_UPLOAD_DPC_EX IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM.
data: lw_file type zfile.
field-symbols:<fs_key> type /iwbep/s_mgw_name_value_pair.
read table it_key_tab assigning <fs_key> index 1.
lw_file-filename = iv_slug.
lw_file-value    = is_media_resource-value.
lw_file-mimetype = is_media_resource-mime_type.
lw_file-sydate  = sy-datum.
lw_file-sytime  = sy-uzeit.

insert into zfile values lw_file.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM.
data :  ls_stream  type ty_s_media_resource,

ls_upld    type zfile,

lv_filename type char30.

field-symbols:<fs_key> type /iwbep/s_mgw_name_value_pair.

read table it_key_tab assigning <fs_key> index 1.
lv_filename = <fs_key>-value.
select single * from zfile into ls_upld where filename = lv_filename.
if ls_upld is not initial.
* DATA: e_xstring  TYPE xstring.
* CALL FUNCTION 'SSFC_BASE64_DECODE'
*    EXPORTING
*      b64data = ls_upld-value
*    IMPORTING
*      bindata = e_xstring
*    EXCEPTIONS
*      OTHERS  = 8.

ls_stream-value = ls_upld-value.
ls_stream-mime_type = ls_upld-mimetype.
copy_data_to_ref( exporting is_data = ls_stream   changing  cr_data = er_stream ).

endif.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_STREAM.
data: lw_file type zfile.
field-symbols:<fs_key> type /iwbep/s_mgw_name_value_pair.

read table it_key_tab assigning <fs_key> index 1.
lw_file-filename = <fs_key>-value.
lw_file-value    = is_media_resource-value.
lw_file-mimetype = is_media_resource-mime_type.
lw_file-sydate  = sy-datum.
lw_file-sytime  = sy-uzeit.
modify zfile from lw_file.
  endmethod.


  method FILESET_GET_ENTITYSET.
 DATA:
      it_final   TYPE STANDARD TABLE OF zfile,
      lt_filters TYPE                   /iwbep/t_mgw_select_option,
      ls_filter  TYPE                   /iwbep/s_mgw_select_option,
      ls_so      TYPE                   /iwbep/s_cod_select_option,
      p_name     TYPE c LENGTH 15.

  SELECT
    mandt
    filename
    SYDATE
    SYTIME
    VALUE
    MIMETYPE

  FROM zfile
  INTO TABLE et_entityset.
  endmethod.
ENDCLASS.
