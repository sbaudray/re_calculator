open React;

type operator =
  | Add
  | Divide
  | Multiply
  | Substract;

type nonZeroDigit =
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Height
  | Nine;

type input =
  | Zero
  | Digit(nonZeroDigit)
  | DecimalSeparator
  | Operator(operator)
  | Clear
  | Equals;

let string_of_digit = digit =>
  switch (digit) {
  | One => "1"
  | Two => "2"
  | Three => "3"
  | Four => "4"
  | Five => "5"
  | Six => "6"
  | Seven => "7"
  | Height => "8"
  | Nine => "9"
  };

type number = float;

type accumulator = string;

type pendingOp = (operator, number);

type zeroStateData = option(pendingOp);

type accumulatorStateData = {
  digits: accumulator,
  pendingOp: option(pendingOp),
};

type computedStateData = {
  displayNumber: number,
  pendingOp: option(pendingOp),
};

type state =
  | ZeroState(zeroStateData)
  | AccumulatorState(accumulatorStateData)
  | DecimalAccumulatorState(accumulatorStateData)
  | ComputedState(computedStateData);

type action =
  | Input(input);

let initialState = ZeroState(None);

let handleZeroState = (action, pendingOp) =>
  switch (action) {
  | Input(Zero) => ZeroState(None)
  | Input(Digit(digit)) =>
    AccumulatorState({digits: string_of_digit(digit), pendingOp})
  | Input(DecimalSeparator) =>
    DecimalAccumulatorState({digits: "0.", pendingOp})
  | Input(Operator(_operator)) =>
    ComputedState({displayNumber: 0., pendingOp})
  | Input(Equals) => ZeroState(None)
  | Input(Clear) => ZeroState(None)
  };

let accumulateNonZeroDigit = (digits, digit) =>
  digits ++ string_of_digit(digit);

let accumulateZero = digits => digits ++ string_of_int(0);

let accumulateSeparator = digits => digits ++ ".";

let performMathOperation = ((operator, number), digits) => {
  switch (operator) {
  | Add => number +. digits
  | Multiply => number *. digits
  | Divide => number /. digits
  | Substract => number -. digits
  };
};

let handleCompute = (accumulatorData: accumulatorStateData, operator) => {
  switch (accumulatorData.pendingOp) {
  | Some(pendingOp) =>
    let result =
      performMathOperation(
        pendingOp,
        float_of_string(accumulatorData.digits),
      );
    {displayNumber: result, pendingOp: Some((operator, result))}
    ->ComputedState;
  | None =>
    {
      displayNumber: float_of_string(accumulatorData.digits),
      pendingOp: Some((operator, float_of_string(accumulatorData.digits))),
    }
    ->ComputedState
  };
};

let handleAccumulatorEquality =
    (accumulatorData: accumulatorStateData): accumulatorStateData => {
  switch (accumulatorData.pendingOp) {
  | Some(pendingOp) => {
      digits:
        performMathOperation(
          pendingOp,
          float_of_string(accumulatorData.digits),
        )
        ->Js.Float.toString,
      pendingOp: None,
    }
  | None => accumulatorData
  };
};

let handleAccumulatorState = (action, accumulatorData: accumulatorStateData) =>
  switch (action) {
  | Input(Zero) =>
    {
      digits: accumulateZero(accumulatorData.digits),
      pendingOp: accumulatorData.pendingOp,
    }
    ->AccumulatorState
  | Input(Digit(digit)) =>
    {
      digits: accumulateNonZeroDigit(accumulatorData.digits, digit),
      pendingOp: accumulatorData.pendingOp,
    }
    ->AccumulatorState
  | Input(DecimalSeparator) =>
    {
      digits: accumulateSeparator(accumulatorData.digits),
      pendingOp: accumulatorData.pendingOp,
    }
    ->DecimalAccumulatorState
  | Input(Operator(operator)) => handleCompute(accumulatorData, operator)
  | Input(Equals) =>
    handleAccumulatorEquality(accumulatorData)->AccumulatorState
  | Input(Clear) => ZeroState(None)
  };

let handleDecimalAccumulatorState = (action, accumulatorData) =>
  switch (action) {
  | Input(Zero) =>
    {
      digits: accumulateZero(accumulatorData.digits),
      pendingOp: accumulatorData.pendingOp,
    }
    ->DecimalAccumulatorState
  | Input(Digit(digit)) =>
    {
      digits: accumulateNonZeroDigit(accumulatorData.digits, digit),
      pendingOp: accumulatorData.pendingOp,
    }
    ->DecimalAccumulatorState
  | Input(DecimalSeparator) => DecimalAccumulatorState(accumulatorData)
  | Input(Operator(operator)) => handleCompute(accumulatorData, operator)
  | Input(Equals) =>
    handleAccumulatorEquality(accumulatorData)->DecimalAccumulatorState
  | Input(Clear) => ZeroState(None)
  };

let selfCompute = ((operator, number)) =>
  switch (operator) {
  | Add => number +. number
  | Multiply => number *. number
  | Divide => number /. number
  | Substract => number -. number
  };

let handleComputedEquality = computedData => {
  switch (computedData.pendingOp) {
  | Some(pendingOp) =>
    {displayNumber: selfCompute(pendingOp), pendingOp: None}->ComputedState
  | None => ComputedState(computedData)
  };
};

let handleComputedState = (action, computedData) =>
  switch (action) {
  | Input(Zero) => ZeroState(computedData.pendingOp)
  | Input(Digit(digit)) =>
    {digits: string_of_digit(digit), pendingOp: computedData.pendingOp}
    ->AccumulatorState
  | Input(DecimalSeparator) =>
    {digits: "0.", pendingOp: computedData.pendingOp}->DecimalAccumulatorState
  | Input(Operator(operator)) =>
    {
      displayNumber: computedData.displayNumber,
      pendingOp: Some((operator, computedData.displayNumber)),
    }
    ->ComputedState
  | Input(Equals) => handleComputedEquality(computedData)
  | Input(Clear) => ZeroState(None)
  };

let reducer = (state, action) =>
  switch (state) {
  | ZeroState(pendingOp) => handleZeroState(action, pendingOp)
  | AccumulatorState(accumulatorData) =>
    handleAccumulatorState(action, accumulatorData)
  | DecimalAccumulatorState(accumulatorData) =>
    handleDecimalAccumulatorState(action, accumulatorData)
  | ComputedState(computedData) => handleComputedState(action, computedData)
  };

[@react.component]
let make = () => {
  let (state, dispatch) = useReducer(reducer, initialState);

  let displayValue =
    switch (state) {
    | ZeroState(_data) => "0"
    | AccumulatorState(data) => data.digits
    | DecimalAccumulatorState(data) => data.digits
    | ComputedState({displayNumber}) => Js.Float.toString(displayNumber)
    };

  <div className="calculator">
    <div className="display"> {string(displayValue)} </div>
    <div className="digits">
      <button onClick={_event => dispatch(Input(Zero))}>
        {string("0")}
      </button>
      <button onClick={_event => dispatch(Input(Digit(One)))}>
        {string("1")}
      </button>
      <button onClick={_event => dispatch(Input(Digit(Two)))}>
        {string("2")}
      </button>
      <button onClick={_event => dispatch(Input(Digit(Three)))}>
        {string("3")}
      </button>
      <button onClick={_event => dispatch(Input(Digit(Four)))}>
        {string("4")}
      </button>
      <button onClick={_event => dispatch(Input(Digit(Five)))}>
        {string("5")}
      </button>
      <button onClick={_event => dispatch(Input(Digit(Six)))}>
        {string("6")}
      </button>
      <button onClick={_event => dispatch(Input(Digit(Seven)))}>
        {string("7")}
      </button>
      <button onClick={_event => dispatch(Input(Digit(Height)))}>
        {string("8")}
      </button>
      <button onClick={_event => dispatch(Input(Digit(Nine)))}>
        {string("9")}
      </button>
    </div>
    <div className="operators">
      <button onClick={_event => dispatch(Input(DecimalSeparator))}>
        {string(".")}
      </button>
      <button onClick={_event => dispatch(Input(Operator(Add)))}>
        {string("+")}
      </button>
      <button onClick={_event => dispatch(Input(Operator(Substract)))}>
        {string("-")}
      </button>
      <button onClick={_event => dispatch(Input(Operator(Multiply)))}>
        {string("*")}
      </button>
      <button onClick={_event => dispatch(Input(Operator(Divide)))}>
        {string("/")}
      </button>
      <button onClick={_event => dispatch(Input(Equals))}>
        {string("=")}
      </button>
      <button onClick={_event => dispatch(Input(Clear))}>
        {string("AC")}
      </button>
    </div>
  </div>;
};
