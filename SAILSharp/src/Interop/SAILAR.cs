namespace SAILSharp.Interop {
    using System;
    using System.Runtime.InteropServices;

    public static unsafe class SAILAR {
        public struct OpaqueIdentifier { }
        public struct OpaqueError { }
        public struct OpaqueErrorMessage { }

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SAILDisposeIdentifier", ExactSpelling = true)]
        public static extern void DisposeIdentifier(OpaqueIdentifier* identifier);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SAILCreateIdentifier", ExactSpelling = true)]
        public static extern OpaqueIdentifier* CreateIdentifier(byte* contents, UIntPtr length, in OpaqueError* error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SAILGetErrorMessage", ExactSpelling = true)]
        public static extern OpaqueErrorMessage* GetErrorMessage(OpaqueError* error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SAILGetErrorMessageContents", ExactSpelling = true)]
        public static extern byte* GetErrorMessageContents(OpaqueErrorMessage* message, in UIntPtr length);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SAILDisposeError", ExactSpelling = true)]
        public static extern void DisposeError(OpaqueError* error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SAILDisposeErrorMessage", ExactSpelling = true)]
        public static extern void DisposeErrorMessage(OpaqueErrorMessage* message);
    }
}
