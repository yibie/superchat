# Notes

One line each: what seems missing or wrong in SPEC.md, and why. Nothing is built from this file.
- L2: `save_buffer` leaves a backup file (`calc.el~`) in the project; the model then sees it in `ls`. The specification does not say whether saving makes backups.
