class ZCL_ZBSR_FORM_MPC_EXT_1 definition
  public
  inheriting from ZCL_ZBSR_FORM_MPC
  create public .

public section.

  types:
*    TYPES: BEGIN OF ts_defaults ,
*             recordtyp     TYPE zreq_type,
*             bsrdefaultnav TYPE STANDARD TABLE OF zbsr_s_defaults WITH DEFAULT KEY,
*           END OF ts_defaults.
    BEGIN OF ts_head_item ,
        req_id            TYPE  zreq_id,
        department        TYPE  zdepartment,
        req_type          TYPE  zreq_type,
        req_status        TYPE  zreq_status,
        cost_center       TYPE kostl,
        owner             TYPE  bname,
        managerid         TYPE  bname,
        product_line      TYPE  zproduct_line,
        special_instr     TYPE  zspecial_instr,
        wip_location      TYPE  lgort_d,
        created_by        TYPE  bname,
        bsr_type          TYPE  zbsr_type,
        werks             TYPE werks_d,
        plnnr             TYPE plnnr,
        plnal             TYPE plnal,
        plnnr_alt         TYPE cp_plnnr_a,
        created_on        TYPE  datum,
        changed_by        TYPE  bname,
        changed_on        TYPE  datum,
        sap_material      TYPE  mfrpn,
        specific_inst     TYPE  zspecific_inst,
        completion_status TYPE  zcompletion_status,
        od_no             TYPE  vbeln,
        qty               TYPE  zqty,
        detape            TYPE flag,
        output_material   TYPE mfrpn,
        tecn              TYPE  ztecn,
        tecn_no           TYPE  ztecn_no,
        purge_batch       TYPE  zpurge_batch,
        units_now_ava_at  TYPE  zunits_now_ava_at,
        so_no	            TYPE vbeln,
        destination       TYPE zbsr_longtext,
        reasforincomplete TYPE zbsr_longtext,
        location          TYPE zbsr_longtext,
        addtext           TYPE zbsr_longtext,
        purpose           TYPE zbsr_longtext,
        ownername         TYPE c LENGTH 100,
        manager           TYPE c LENGTH 100,
        hitemno           TYPE c LENGTH 5,
        uom               TYPE c LENGTH 3,
        createdonuser     TYPE c LENGTH 100,
        changedonuser     TYPE c LENGTH 100,
        req_status_sub    TYPE c LENGTH 2,
        relid             TYPE c LENGTH 40,
*        Rec_Werks         TYPE werks_d, //gg8
*        Plnty             TYPE plnty, //gg8
        headitnav         TYPE STANDARD TABLE OF ts_bsritem            WITH DEFAULT KEY,
        itnotifnav        TYPE STANDARD TABLE OF ts_bsrnotifications   WITH DEFAULT KEY,
      END OF ts_head_item .

  class-data GC_OBJECT type TDOBJECT value 'ZBSR_LT' ##NO_TEXT.
  class-data GC_TDID_RES type TDID value 'ZRES' ##NO_TEXT.
  class-data GC_TDID_AT type TDID value 'ZAT' ##NO_TEXT.
  class-data GC_TDID_DES type TDID value 'ZDES' ##NO_TEXT.
  class-data GC_TDID_INC type TDID value 'ZINC' ##NO_TEXT.
  class-data GC_TDID_LOC type TDID value 'ZLOC' ##NO_TEXT.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZBSR_FORM_MPC_EXT_1 IMPLEMENTATION.


  METHOD DEFINE.
    super->define( ).

    DATA: lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property    TYPE REF TO /iwbep/if_mgw_odata_property,
          lo_entity_set  TYPE REF TO /iwbep/if_mgw_odata_entity_set,
          lv_namespace   TYPE string,
          lo_annotation  TYPE REF TO cl_fkk_shlp_annotation.

    lo_entity_type = model->get_entity_type( iv_entity_name = 'BSRHeader' ). "#EC NOTEXT
    lo_entity_type->bind_structure( iv_structure_name  = 'ZCL_ZBSR_FORM_MPC_EXT=>TS_HEAD_ITEM' ). "#EC NOTEXT
    model->set_soft_state_enabled( iv_soft_state_enabled = abap_true ).
    model->get_schema_namespace( IMPORTING ev_namespace = lv_namespace ).

    "Plant Search Help Annotation
    lo_annotation = cl_fkk_shlp_annotation=>create(
        io_odata_model               = model
        io_vocan_model               = vocab_anno_model
        iv_namespace                 = lv_namespace
        iv_entitytype                = 'BSRHeader'
        iv_property                  = 'Werks'
        iv_search_help               = 'H_T001W'
        iv_search_help_field         = 'WERKS'
        iv_valuelist_entityset       = 'PlantSHSet'
        iv_valuelist_property        = 'Werks' ).

    lo_annotation->add_display_parameter( iv_valuelist_property  = 'Plnt' ) .
    lo_annotation->add_display_parameter( iv_valuelist_property  = 'SearchTerm2' ) .
    lo_annotation->add_display_parameter( iv_valuelist_property  = 'SearchTerm1' ) .
    lo_annotation->add_display_parameter( iv_valuelist_property  = 'PostlCode' ) .
    lo_annotation->add_display_parameter( iv_valuelist_property  = 'City' ) .
    lo_annotation->add_display_parameter( iv_valuelist_property  = 'Name2' ) .
    lo_annotation->add_display_parameter( iv_valuelist_property  = 'Name1' ) .
    lo_annotation->add_display_parameter( iv_valuelist_property  = 'Version' ) .
  ENDMETHOD.
ENDCLASS.
