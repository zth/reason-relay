type reasonAst;

[@bs.module "reason"] external parseRE: string => reasonAst = "parseRE";

[@bs.module "reason"] external printRE: reasonAst => string = "printRE";