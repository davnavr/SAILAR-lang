namespace SAILARSharp.Interop {
    using System;
    using System.Runtime.InteropServices;

    public struct OpaqueIdentifier { }
    public struct OpaqueError { }
    public struct OpaqueErrorMessage { }
    public struct OpaqueBuffer { }
    public struct OpaqueTypeSignature { }
    public struct OpaqueModuleReader { }
    public struct OpaqueModuleFormat { }
    public struct OpaqueModuleRecord { }
    public struct OpaqueModuleBuilder { }

    internal static unsafe class SAILAR {
        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_identifier", ExactSpelling = true)]
        public static extern void DisposeIdentifier(OpaqueIdentifier* identifier);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_identifier", ExactSpelling = true)]
        public static extern OpaqueIdentifier* CreateIdentifier(byte* contents, UIntPtr length, OpaqueError** error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_identifier_contents", ExactSpelling = true)]
        public static extern byte* GetIdentifierContents(OpaqueIdentifier* Identifier, UIntPtr* length);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_error_message", ExactSpelling = true)]
        public static extern OpaqueErrorMessage* GetErrorMessage(OpaqueError* error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_error_message_contents", ExactSpelling = true)]
        public static extern byte* GetErrorMessageContents(OpaqueErrorMessage* message, UIntPtr* length);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_error", ExactSpelling = true)]
        public static extern void DisposeError(OpaqueError* error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_error_message", ExactSpelling = true)]
        public static extern void DisposeErrorMessage(OpaqueErrorMessage* message);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_create_buffer_from_address", ExactSpelling = true)]
        public static extern OpaqueBuffer* CreateBufferFromAddress(byte* address, UIntPtr length);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_buffer", ExactSpelling = true)]
        public static extern void DisposeBuffer(OpaqueBuffer* buffer);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_buffer_contents", ExactSpelling = true)]
        public static extern byte* GetBufferContents(OpaqueBuffer* buffer, UIntPtr* length);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_type_signature_kind", ExactSpelling = true)]
        public static extern TypeSignatureKind GetTypeSignatureKind(OpaqueTypeSignature* signature);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_type_signature", ExactSpelling = true)]
        public static extern void DisposeTypeSignature(OpaqueTypeSignature* signature);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_create_module_reader_from_buffer", ExactSpelling = true)]
        public static extern OpaqueModuleReader* CreateModuleReaderFromBuffer(OpaqueBuffer* buffer);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_create_module_reader_from_path", ExactSpelling = true)]
        public static extern OpaqueModuleReader* CreateModuleReaderFromPath(byte* path, UIntPtr length, OpaqueError** error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_module_reader", ExactSpelling = true)]
        public static extern void DisposeModuleReader(OpaqueModuleReader* reader);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_check_module_reader_finished", ExactSpelling = true)]
        public static extern void CheckModuleReaderFinished(OpaqueModuleReader* reader, OpaqueError** error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_module_format_major_version", ExactSpelling = true)]
        public static extern byte GetModuleFormatMajorVersion(OpaqueModuleFormat* format);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_module_format_minor_version", ExactSpelling = true)]
        public static extern byte GetModuleFormatMinorVersion(OpaqueModuleFormat* format);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_module_format_integer_byte_size", ExactSpelling = true)]
        public static extern byte GetModuleFormatIntegerByteSize(OpaqueModuleFormat* format);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_module_format", ExactSpelling = true)]
        public static extern void DisposeModuleFormat(OpaqueModuleFormat* format);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_read_module_format", ExactSpelling = true)]
        public static extern OpaqueModuleFormat* ReadModuleFormat(OpaqueModuleReader* reader, OpaqueError** error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_read_module_next_record", ExactSpelling = true)]
        public static extern OpaqueModuleRecord* ReadModuleNextRecord(OpaqueModuleReader* reader, OpaqueError** error);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_module_record_type", ExactSpelling = true)]
        public static extern RecordType GetModuleRecordType(OpaqueModuleRecord* format);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_module_record_as_identifier", ExactSpelling = true)]
        public static extern OpaqueIdentifier* GetModuleRecordAsIdentifier(OpaqueModuleRecord* format);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_get_module_record_as_type_signature", ExactSpelling = true)]
        public static extern OpaqueTypeSignature* GetModuleRecordAsTypeSignature(OpaqueModuleRecord* format);

        [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_module_record", ExactSpelling = true)]
        public static extern void DisposeModuleRecord(OpaqueModuleRecord* format);
    }
}
