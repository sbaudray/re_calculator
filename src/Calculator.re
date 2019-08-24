open React;

type operator =
  | Add
  | Divide
  | Multiply
  | Substract
  | Equal;

type digit =
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Height
  | Nine;

let digitToInt = digit =>
  switch (digit) {
  | One => 1
  | Two => 2
  | Three => 3
  | Four => 4
  | Five => 5
  | Six => 6
  | Seven => 7
  | Height => 8
  | Nine => 9
  };

let operatorToString = operator =>
  switch (operator) {
  | Add => "+"
  | Divide => "/"
  | Multiply => "*"
  | Substract => "-"
  | Equal => "="
  };

type state =
  | ZeroState
  | DigitState(string)
  | ComputeState(string, operator);

type action =
  | InputZero
  | InputDigit(digit)
  | InputOperator(operator);

let initialState = ZeroState;

let handleActionForInputZero = state =>
  switch (state) {
  | ZeroState => ZeroState
  | DigitState(displayValue) => DigitState(displayValue ++ string_of_int(0))
  | ComputeState(_displayValue, _operator) => state
  };

let handleActionForInputDigit = (state, digit) => {
  let digitStr = digit->digitToInt->string_of_int;

  switch (state) {
  | ZeroState => DigitState(digitStr)
  | DigitState(displayValue) => DigitState(displayValue ++ digitStr)
  | ComputeState(_displayValue, _operator) => state
  };
};

let handleActionForInputOperator = (state, operator) => {
  switch (state) {
  | ZeroState => ComputeState("0", operator)
  | DigitState(displayValue) => ComputeState(displayValue, operator)
  | ComputeState(displayValue, _operator) =>
    ComputeState(displayValue, operator)
  };
};

let reducer = (state, action) =>
  switch (action) {
  | InputZero => handleActionForInputZero(state)
  | InputDigit(digit) => handleActionForInputDigit(state, digit)
  | InputOperator(operator) => handleActionForInputOperator(state, operator)
  };

let displayValue = state =>
  switch (state) {
  | ZeroState => "0"
  | DigitState(displayValue) => displayValue
  | ComputeState(displayValue, _operator) => displayValue
  };

let pendingOp = state =>
  switch (state) {
  | ComputeState(_displayValue, operator) => operatorToString(operator)
  | _ => ""
  };

[@react.component]
let make = () => {
  let (state, dispatch) = useReducer(reducer, initialState);

  <div className="calculator">
    <div> {string(pendingOp(state))} </div>
    <div className="display"> {string(displayValue(state))} </div>
    <div className="digits">
      <button onClick={_event => dispatch(InputZero)}>
        {string("0")}
      </button>
      <button onClick={_event => dispatch(InputDigit(One))}>
        {string("1")}
      </button>
      <button onClick={_event => dispatch(InputDigit(Two))}>
        {string("2")}
      </button>
      <button onClick={_event => dispatch(InputDigit(Three))}>
        {string("3")}
      </button>
      <button onClick={_event => dispatch(InputDigit(Four))}>
        {string("4")}
      </button>
      <button onClick={_event => dispatch(InputDigit(Five))}>
        {string("5")}
      </button>
      <button onClick={_event => dispatch(InputDigit(Six))}>
        {string("6")}
      </button>
      <button onClick={_event => dispatch(InputDigit(Seven))}>
        {string("7")}
      </button>
      <button onClick={_event => dispatch(InputDigit(Height))}>
        {string("8")}
      </button>
      <button onClick={_event => dispatch(InputDigit(Nine))}>
        {string("9")}
      </button>
    </div>
    <div className="operators">
      <button onClick={_event => dispatch(InputOperator(Add))}>
        {string("+")}
      </button>
      <button onClick={_event => dispatch(InputOperator(Substract))}>
        {string("-")}
      </button>
      <button onClick={_event => dispatch(InputOperator(Multiply))}>
        {string("*")}
      </button>
      <button onClick={_event => dispatch(InputOperator(Divide))}>
        {string("/")}
      </button>
      <button onClick={_event => dispatch(InputOperator(Equal))}>
        {string("=")}
      </button>
    </div>
  </div>;
};
