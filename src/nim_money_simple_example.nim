import std/[strutils, math, sequtils]

type
  CurrencyMismatchError* = object of ValueError
  InvalidCurrencyError* = object of ValueError

  Currency* = object
    code*: string        ## ISO-4217 3-letter code (e.g., "USD")
    numericCode*: int    ## ISO-4217 numeric code (e.g., 840)
    minorUnit*: int      ## Exponent / number of sub-unit decimals (e.g., 2 for USD, 0 for JPY, 3 for KWD)
    symbol*: string      ## Display symbol (e.g., "$")

  Money* = object
    amount*: int64       ## Stored strictly in minor units (e.g., cents, satoshis)
    currency*: Currency

# --- ISO-4217 Currency Definitions ---
const
  USD* = Currency(code: "USD", numericCode: 840, minorUnit: 2, symbol: "$")
  EUR* = Currency(code: "EUR", numericCode: 978, minorUnit: 2, symbol: "€")
  GBP* = Currency(code: "GBP", numericCode: 826, minorUnit: 2, symbol: "£")
  JPY* = Currency(code: "JPY", numericCode: 392, minorUnit: 0, symbol: "¥")
  CHF* = Currency(code: "CHF", numericCode: 756, minorUnit: 2, symbol: "CHF ")
  CAD* = Currency(code: "CAD", numericCode: 124, minorUnit: 2, symbol: "CA$")
  AUD* = Currency(code: "AUD", numericCode: 036, minorUnit: 2, symbol: "A$")
  KWD* = Currency(code: "KWD", numericCode: 414, minorUnit: 3, symbol: "KD ")

# --- Constructors ---
proc initMoney*(amount: int64, currency: Currency): Money {.inline.} =
  ## Amount must be provided in minor units (e.g., 1000 = $10.00 USD)
  Money(amount: amount, currency: currency)

# Helper for type safety
proc checkSameCurrency(a, b: Money) {.inline.} =
  if a.currency.code != b.currency.code:
    raise newException(CurrencyMismatchError, 
      "Currency mismatch: " & a.currency.code & " and " & b.currency.code)

# --- Operator Overloading ---
proc `+`*(a, b: Money): Money =
  checkSameCurrency(a, b)
  Money(amount: a.amount + b.amount, currency: a.currency)

proc `-`*(a, b: Money): Money =
  checkSameCurrency(a, b)
  Money(amount: a.amount - b.amount, currency: a.currency)

proc `*`*(m: Money, multiplier: int64): Money =
  Money(amount: m.amount * multiplier, currency: m.currency)

proc `*`*(multiplier: int64, m: Money): Money =
  m * multiplier

proc `div`*(m: Money, divisor: int64): Money =
  if divisor == 0:
    raise newException(DivByZeroError, "Cannot divide Money by zero")
  Money(amount: m.amount div divisor, currency: m.currency)

# --- Comparisons ---
proc `==`*(a, b: Money): bool =
  a.currency.code == b.currency.code and a.amount == b.amount

proc `<`*(a, b: Money): bool =
  checkSameCurrency(a, b)
  a.amount < b.amount

proc `<=`*(a, b: Money): bool =
  checkSameCurrency(a, b)
  a.amount <= b.amount

proc isZero*(m: Money): bool {.inline.} = m.amount == 0
proc isPositive*(m: Money): bool {.inline.} = m.amount > 0
proc isNegative*(m: Money): bool {.inline.} = m.amount < 0
proc abs*(m: Money): Money {.inline.} = Money(amount: abs(m.amount), currency: m.currency)

# --- Fowler's Allocation Algorithm ---
proc allocate*(m: Money, ratios: openArray[int]): seq[Money] =
  ## Distributes Money across ratios without losing minor units (pennies).
  if ratios.len == 0:
    raise newException(ValueError, "Ratios array cannot be empty")
  
  let totalRatio = sum(ratios)
  if totalRatio <= 0:
    raise newException(ValueError, "Sum of ratios must be greater than zero")

  result = newSeq[Money](ratios.len)
  var remainder = m.amount

  # 1. Floor distribution
  for i, ratio in ratios:
    let share = (m.amount * ratio.int64) div totalRatio.int64
    result[i] = Money(amount: share, currency: m.currency)
    remainder -= share

  # 2. Distribute leftover minor units starting from the first party
  var i = 0
  while remainder > 0:
    result[i].amount += 1
    remainder -= 1
    i = (i + 1) mod ratios.len

# --- String Formatting ---
proc format*(m: Money, includeSymbol: bool = true, thousandSep: char = ','): string =
  ## Formats money into human-readable currency representation.
  let isNeg = m.amount < 0
  let absAmt = abs(m.amount)
  let scale = 10 ^ m.currency.minorUnit

  var mainStr: string
  var fracStr: string

  if m.currency.minorUnit == 0:
    mainStr = $absAmt
    fracStr = ""
  else:
    let mainPart = absAmt div scale
    let fracPart = absAmt mod scale
    mainStr = $mainPart
    fracStr = $fracPart
    # Pad leading zeros for fractional parts (e.g. 5 cents -> "05")
    while fracStr.len < m.currency.minorUnit:
      fracStr = "0" & fracStr

  # Apply thousands separators
  var formattedMain = ""
  let length = mainStr.len
  for i, ch in mainStr:
    formattedMain.add(ch)
    let revIdx = length - 1 - i
    if revIdx > 0 and revIdx mod 3 == 0 and thousandSep != '\0':
      formattedMain.add(thousandSep)

  var body = formattedMain
  if m.currency.minorUnit > 0:
    body.add('.' & fracStr)

  let prefix = if isNeg: "-" else: ""
  let sym = if includeSymbol: m.currency.symbol else: ""

  return prefix & sym & body

proc `$`*(m: Money): string =
  m.format(includeSymbol = true)

# --- Verification & Unit Test Block ---
when isMainModule:
  echo "=== Running Money Module Tests ==="

  # Basic Arithmetic
  let m1 = initMoney(10000, USD) # $100.00
  let m2 = initMoney(2550, USD)  # $25.50

  assert $(m1 + m2) == "$125.50"
  assert $(m1 - m2) == "$74.50"
  assert $(m2 * 2) == "$51.00"

  # Zero-decimal and Three-decimal Currencies
  let yen = initMoney(1250000, JPY)
  assert $yen == "¥1,250,000"

  let dinar = initMoney(12345, KWD)
  assert $dinar == "KD 12.345"

  # Fowler Allocation Test ($100.00 split 1:1:1)
  let shares = m1.allocate([1, 1, 1])
  assert shares[0].amount == 3334 # $33.34
  assert shares[1].amount == 3333 # $33.33
  assert shares[2].amount == 3333 # $33.33
  assert shares[0].amount + shares[1].amount + shares[2].amount == 10000 # Total intact

  # Currency Mismatch Protection
  try:
    discard m1 + initMoney(100, EUR)
    assert false, "Should have thrown CurrencyMismatchError"
  except CurrencyMismatchError:
    echo "✓ Currency mismatch protection verified."

  echo "✓ All tests passed successfully!"