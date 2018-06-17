# bc_commodities

Money, Credit & Finance paper dealing with business cycles in resource dependent economies.

# To-Do

- vary transformations (HP, first diff, log...)
- add stock indices to data
- add commodities in different forms


# Literature Review

### Mallick & Sousa 2002

- Five BRICS countries (BR, RU, IN, CN, ZA)
- Evaluate transmission of monetary policy & fluctuations in commodity prices

##### Econometric Methodology
Standard SVAR with recursive identification: X is split into three groups:
- n1 variables that respond to a monetary policy shock with lag (GDP deflator, real GDP, commodity price index)
- n2 variables that adjust contemporaneously (growth of M2, real effective exchange rate, equity price index)
- the policy instrument (nominal central bank rate)

##### Data
Variables are expressed in logs of first-differences.
- Commodity price index
- Inflation rate (computed from country-specific CPIs)
- Real GDP (GDP at constant prices)
- Nominal central bank rate (Monetary policy instrument)
- Broad money supply M2
- Real effective exchange rate
- Stock price index

##### Findings
Commodity price shocks lead to a rise in inflation & demand aggressive behaviour from central banks to stabilise.