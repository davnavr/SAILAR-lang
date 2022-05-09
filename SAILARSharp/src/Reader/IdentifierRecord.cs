namespace SAILARSharp.Reader {
    using SAILARSharp.Interop;

    public unsafe sealed class IdentifierRecord : ModuleRecord {
        private OpaqueIdentifier* identifier;
        private string? contents = null;

        internal IdentifierRecord(OpaqueModuleRecord* record) : base(record) {
            identifier = SAILAR.GetModuleRecordAsIdentifier(record);
        }

        public OpaqueIdentifier* Content {
            get {
                ThrowIfDisposed();
                return identifier;
            }
        }

        public sealed override string ToString() {
            if (contents == null) {
                contents = Identifier.ToString(identifier);
            }

            return contents;
        }

        private protected override void Cleanup() {
            SAILAR.DisposeIdentifier(identifier);
            identifier = null;
        }
    }
}
