

#ifndef ERRORS_H
#define ERRORS_H

void scwm_error(char *subr, int err);
void scwm_error_imm(char *subr, const char *szErrMsg);


#define ERROR_LOAD_PICTURE 14

#endif /* ERRORS_H */
