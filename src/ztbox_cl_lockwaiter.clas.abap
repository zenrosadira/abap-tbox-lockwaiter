class ZTBOX_CL_LOCKWAITER definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !I_TABLE type ROOTTAB .
  methods SET_OBJECT
    importing
      !IS_KEYS type ANY .
  methods WAIT_FOR
    importing
      !I_ENDLESSLY type FLAG optional .
  methods GET_ERRORS
    returning
      value(R_ERRS) type STRING_TABLE .
  methods LOCK
    importing
      !I_ENQ_MODE type C default 'E' .
  methods UNLOCK .
protected section.
private section.

  types:
    BEGIN OF ty_key_fields,
           fieldname TYPE feld_name,
           value     TYPE string,
         END OF ty_key_fields .
  types:
    ty_key_fields_t TYPE TABLE OF ty_key_fields WITH KEY fieldname .

  data _ROOT_TAB type ROOTTAB .
  data _DEQUEUE_CALLER type ref to ZTBOX_CL_FMODULER .
  data _ENQUEUE_CALLER type ref to ZTBOX_CL_FMODULER .
  data _ERRORS type STRING_TABLE .
  data _ENQ_MODE type C .
  data _KEY_FIELDS type TY_KEY_FIELDS_T .

  methods _SET_FUNCTIONS .
  methods _ADD_ERROR
    importing
      !I_ERR type STRING optional .
  methods _GET_FIELDS_LIST
    returning
      value(R_FIELDS) type DDFIELDS .
  methods _SET_KEY_FIELDS .
  methods _MAP_KEYS
    importing
      !IS_KEYS type ANY .
ENDCLASS.



CLASS ZTBOX_CL_LOCKWAITER IMPLEMENTATION.


  METHOD constructor.

    _root_tab = i_table.

    _set_functions( ).

    _set_key_fields( ).

  ENDMETHOD.


  METHOD get_errors.

    r_errs = _errors.

  ENDMETHOD.


  METHOD lock.

    _enqueue_caller->free( ).

    _enq_mode = i_enq_mode.

    LOOP AT _key_fields INTO DATA(key).

      _enqueue_caller->get_param( key-fieldname )->set_value( key-value ).

    ENDLOOP.

    _enqueue_caller->get_param( |MODE_{ _root_tab }| )->set_value( i_enq_mode ).

    _enqueue_caller->execute( ).

    DATA(foreign_lock) = _enqueue_caller->exception( ).

    _errors = COND #( WHEN foreign_lock IS NOT INITIAL THEN VALUE #( ( foreign_lock-message ) ) ELSE VALUE #( ) ).

  ENDMETHOD.


  METHOD unlock.

    _dequeue_caller->free( ).

    LOOP AT _key_fields INTO DATA(key).

      _dequeue_caller->get_param( key-fieldname )->set_value( key-value ).

    ENDLOOP.

    _dequeue_caller->get_param( |MODE_{ _root_tab }| )->set_value( _enq_mode ).

    _dequeue_caller->execute( ).

    DATA(foreign_lock) = _dequeue_caller->exception( ).

    _errors = COND #( WHEN foreign_lock IS NOT INITIAL THEN VALUE #( ( foreign_lock-message ) ) ELSE VALUE #( ) ).

  ENDMETHOD.


  METHOD wait_for.

    _enqueue_caller->free( ).

    LOOP AT _key_fields INTO DATA(key).

      _enqueue_caller->get_param( key-fieldname )->set_value( key-value ).

    ENDLOOP.

    _enqueue_caller->get_param( |MODE_{ _root_tab }| )->set_value( |V| ).
    _enqueue_caller->get_param( |_WAIT| )->set_value( abap_true ).

    IF i_endlessly EQ abap_true.

      DO.
        CLEAR _errors.
        _enqueue_caller->execute( ).
        DATA(foreign_lock) = _enqueue_caller->exception( ).
        IF foreign_lock IS INITIAL.
          EXIT.
        ENDIF.
      ENDDO.

    ELSE.

      _enqueue_caller->execute( ).
      foreign_lock = _enqueue_caller->exception( ).
      _errors = COND #( WHEN foreign_lock IS NOT INITIAL THEN VALUE #( ( foreign_lock-message ) ) ELSE VALUE #( ) ).

    ENDIF.

  ENDMETHOD.


  METHOD _add_error.

    IF i_err IS SUPPLIED.

      APPEND i_err TO _errors.

    ELSE.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_msg).
      APPEND lv_msg TO _errors.

    ENDIF.

  ENDMETHOD.


  METHOD set_object.

    _map_keys( is_keys ).

  ENDMETHOD.


  METHOD _get_fields_list.

    CLEAR r_fields.

    DATA(table_descr) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( _root_tab ) ).
    CHECK table_descr IS BOUND.

    table_descr->get_ddic_field_list(
      RECEIVING
        p_field_list = r_fields
      EXCEPTIONS
        no_ddic_type  = 1
        not_found     = 2 ).
    IF sy-subrc NE 0.
      _add_error( ).
    ENDIF.

    DELETE r_fields WHERE domname EQ 'MANDT'.

  ENDMETHOD.


  METHOD _map_keys.

    LOOP AT _key_fields ASSIGNING FIELD-SYMBOL(<key>).

      CLEAR <key>-value.

      ASSIGN COMPONENT <key>-fieldname OF STRUCTURE is_keys TO FIELD-SYMBOL(<key_value>).
      CHECK sy-subrc EQ 0.

      <key>-value = <key_value>.

    ENDLOOP.

  ENDMETHOD.


  METHOD _SET_FUNCTIONS.

    SELECT SINGLE viewname
      FROM dd25l
      WHERE roottab   EQ @_root_tab
        AND as4local  EQ 'A'
        AND as4vers   EQ '0000'
        AND aggtype   EQ 'E'
       INTO @DATA(enqueue).
    IF sy-subrc NE 0.
      _add_error( |{ _root_tab } not found.| ).
      RETURN.
    ENDIF.

    _enqueue_caller = NEW ztbox_cl_fmoduler( |ENQUEUE_{ enqueue }| ).
    _dequeue_caller = NEW ztbox_cl_fmoduler( |DEQUEUE_{ enqueue }| ).

  ENDMETHOD.


  METHOD _set_key_fields.

    DATA(fields_list) = _get_fields_list( ).

    _key_fields = VALUE #( FOR field IN fields_list WHERE ( keyflag EQ abap_true )
      ( fieldname = field-fieldname ) ).

  ENDMETHOD.
ENDCLASS.
