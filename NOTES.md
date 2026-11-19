# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- L2: a request with no tool spec makes `deepseek-flash` write its own call syntax as text (`<｜｜DSML｜｜ invoke …>`), which reaches the person. The specification does not say what a bridge does with call syntax that arrives as text.
