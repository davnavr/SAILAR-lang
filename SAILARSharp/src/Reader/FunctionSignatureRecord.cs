namespace SAILARSharp.Reader {
    using SAILARSharp;
    using SAILARSharp.Interop;

    public unsafe sealed class FunctionSignatureRecord : ModuleRecord {
        private readonly FunctionSignature signature;

        internal FunctionSignatureRecord(OpaqueModuleRecord* record) : base(record, RecordType.TypeSignature) {
            signature = new(SAILAR.GetModuleRecordAsFunctionSignature(record));
        }

        public FunctionSignature Content {
            get {
                ThrowIfDisposed();
                return signature;
            }
        }

        private protected override void Cleanup() {
            signature.Dispose();
        }
    }
}
