This task calculates sampling weights at the hospital-ZIP-ER and hospital-ZIP-ER-DRG levels. 
With a 20% sample of beneficiaries (defined in `extract_beneficiary_sample`), the formula
for the inverse sampling probability is given by the binomial theorem: $ \frac{1}{1 - 0.8^n} $.
In the full sample, `n` would be the observed count of beneficiaries in each hospital-ZIP-ER(-DRG) tuple. 
Here, `n` is approximated by taking the sampled count of beneficiaries and multiplying by five.
