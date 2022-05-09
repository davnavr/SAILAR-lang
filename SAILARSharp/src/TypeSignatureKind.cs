namespace SAILARSharp {
    public enum TypeSignatureKind : byte {
        U8 = 1,
        U16 = 2,
        U32 = 4,
        U64 = 8,
        UPtr = 0xA,
        S8 = 0x11,
        S16 = 0x12,
        S32 = 0x14,
        S64 = 0x18,
        SPtr = 0x1A,
        RawPtr = 0xCA,
        VoidPtr = 0xCC,
        FuncPtr = 0xCF,
        F32 = 0xF4,
        F64 = 0xF8,
    }
}
