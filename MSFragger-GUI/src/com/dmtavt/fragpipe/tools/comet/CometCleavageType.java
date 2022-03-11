package com.dmtavt.fragpipe.tools.comet;

public enum CometCleavageType {
    ENZYMATIC(2),
    SEMI(1),
    SEMI_N_TERM(9),
    SEMI_C_TERM(8);

    private final int numEnzymeTermini;

    CometCleavageType(int num_enzyme_termini) {
        this.numEnzymeTermini = num_enzyme_termini;
    }

    public int valueInParamsFile() {
        return numEnzymeTermini;
    }

    public static CometCleavageType fromValueInParamsFile(String paramsFileRepresentation) {
        for (CometCleavageType ct : CometCleavageType.values()) {
            if (Integer.toString(ct.valueInParamsFile()).equals(paramsFileRepresentation)) {
                return ct;
            }
        }
        throw new IllegalArgumentException(
                "Enum CometCleavageType does not contain a mapping for params file value of '"
                        + paramsFileRepresentation + "'");
    }
}