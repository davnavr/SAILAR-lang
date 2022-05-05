namespace SAILSharp.Interop {
    using System;
    using System.Runtime.InteropServices;

    public struct OpaqueIdentifier { }
    public struct OpaqueError { }
    public struct OpaqueErrorMessage { }
    public struct OpaqueModuleReader { }
    public struct OpaqueRecordReader { }

    public static unsafe class SAILAR {
        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_identifier", ExactSpelling = true)]
        public static extern void DisposeIdentifier(OpaqueIdentifier* identifier);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_identifier", ExactSpelling = true)]
        public static extern OpaqueIdentifier* CreateIdentifier(byte* contents, UIntPtr length, in OpaqueError* error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_identifier_contents", ExactSpelling = true)]
        public static extern byte* GetIdentifierContents(OpaqueIdentifier* Identifier, in UIntPtr length);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_error_message", ExactSpelling = true)]
        public static extern OpaqueErrorMessage* GetErrorMessage(OpaqueError* error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_error_message_contents", ExactSpelling = true)]
        public static extern byte* GetErrorMessageContents(OpaqueErrorMessage* message, in UIntPtr length);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_error", ExactSpelling = true)]
        public static extern void DisposeError(OpaqueError* error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_error_message", ExactSpelling = true)]
        public static extern void DisposeErrorMessage(OpaqueErrorMessage* message);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_create_module_reader_from_buffer", ExactSpelling = true)]
        public static extern OpaqueModuleReader* CreateModuleReaderFromBuffer(byte* buffer, UIntPtr length);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_records_from_module_reader", ExactSpelling = true)]
        public static extern OpaqueRecordReader* GetRecordsFromModuleReader(OpaqueModuleReader* reader, in byte majorFormatVersion, in byte minorFormatVersion, in byte integerByteSize, in OpaqueError* error);
    }
}
