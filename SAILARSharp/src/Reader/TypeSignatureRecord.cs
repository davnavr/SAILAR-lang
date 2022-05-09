namespace SAILARSharp.Reader {
    using SAILARSharp;
    using SAILARSharp.Interop;

    public unsafe sealed class TypeSignatureRecord : ModuleRecord {
        private readonly TypeSignature signature;

        internal TypeSignatureRecord(OpaqueModuleRecord* record) : base(record) {
            signature = new(SAILAR.GetModuleRecordAsTypeSignature(record));
        }

        public TypeSignature Content {
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
