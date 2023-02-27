class ZTBOX_CL_LOCKWAITER definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !I_TABLE type ROOTTAB .
  methods WAIT_FOR
    importing
      !IS_KEY type ANY
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

  data _ROOT_TAB type ROOTTAB .
  data _ENQUEUE_CALLER type ref to ZTBOX_CL_FMODULER .
  data _ERRORS type STRING_TABLE .
  data _ENQ_MODE type ENQMODE .

  methods _SET_ENQUEUE .
  methods _ADD_ERROR
    importing
      !I_ERR type STRING optional .
ENDCLASS.



CLASS ZTBOX_CL_LOCKWAITER IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    _root_tab = i_table.

  ENDMETHOD.


  METHOD get_errors.

    r_errs = _errors.

  ENDMETHOD.


  METHOD lock.

    DATA(lo_locker) = NEW ztbox_cl_fmoduler( 'ENQUEUE_E_TABLE' ).

    lo_locker->add_parameter(
      i_param_name  = 'TABNAME'
      i_param_value = _root_tab ).

    lo_locker->add_parameter(
      i_param_name  = 'MODE_RSTABLE'
      i_param_value = i_enq_mode ).

    _enq_mode = i_enq_mode.

    lo_locker->execute( ).

    _errors = lo_locker->get_errors( ).

  ENDMETHOD.


  METHOD unlock.

    DATA(lo_unlocker) = NEW ztbox_cl_fmoduler( 'DEQUEUE_E_TABLE' ).

    lo_unlocker->add_parameter(
      i_param_name  = 'TABNAME'
      i_param_value = _root_tab ).

    lo_unlocker->add_parameter(
      i_param_name  = 'MODE_RSTABLE'
      i_param_value = _enq_mode ).

    lo_unlocker->execute( ).

    _errors = lo_unlocker->get_errors( ).

  ENDMETHOD.


  METHOD wait_for.

    _set_enqueue( ).
    CHECK _enqueue_caller IS BOUND.

    DATA(lo_tab) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( _root_tab ) ).
    CHECK lo_tab IS BOUND.

    lo_tab->get_ddic_field_list(
      RECEIVING
        p_field_list = DATA(lt_tab_field)
      EXCEPTIONS
        no_ddic_type  = 1
        not_found     = 2 ).
    IF sy-subrc NE 0.
      _add_error( ).
      RETURN.
    ENDIF.

    LOOP AT lt_tab_field INTO DATA(ls_key) WHERE keyflag EQ abap_true.
      ASSIGN COMPONENT ls_key-fieldname OF STRUCTURE is_key TO FIELD-SYMBOL(<fs_key>).
      CHECK sy-subrc EQ 0.

      _enqueue_caller->add_parameter(
        i_param_name  = CONV string( ls_key-fieldname )
        i_param_value = <fs_key> ).
    ENDLOOP.

    _enqueue_caller->add_parameter(
      i_param_name  = |MODE_{ _root_tab }|
      i_param_value = 'V' ).

    _enqueue_caller->add_parameter(
      i_param_name  = '_WAIT'
      i_param_value = abap_true ).

    IF i_endlessly EQ abap_true.

      DO.
        CLEAR _errors.
        _enqueue_caller->execute( ).
        _errors = _enqueue_caller->get_errors( ).
        IF _errors IS INITIAL.
          EXIT.
        ENDIF.
      ENDDO.

    ELSE.

      _enqueue_caller->execute( ).
      _errors = _enqueue_caller->get_errors( ).

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


  METHOD _set_enqueue.

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

  ENDMETHOD.
ENDCLASS.
