namespace SAILSharp.Interop {
    using System;
    using System.Text;

    // TODO: Make these just static methods, too easy to forget Dispose call.
    public unsafe ref struct Identifier {
        private OpaqueIdentifier* identifier;

        public Identifier(OpaqueIdentifier* identifier) {
            this.identifier = identifier;
        }

        public Identifier(string contents) {
            byte[] bytes = Encoding.UTF8.GetBytes(contents);
            OpaqueError* error;
            fixed (byte* address = bytes) {
                identifier = SAILAR.CreateIdentifier(address, (UIntPtr)bytes.Length, &error);
            }

            Error.HandleError(error);
        }

        public static Identifier Null => default;

        public OpaqueIdentifier* Reference => identifier;

        public override string ToString() {
            if (identifier is null) {
                return string.Empty;
            }

            var length = UIntPtr.Zero;
            var content = SAILAR.GetIdentifierContents(identifier, &length);
            return Encoding.UTF8.GetString(content, (int)length);
        }

        public void Dispose() {
            if (identifier != null) {
                SAILAR.DisposeIdentifier(identifier);
                identifier = null;
            }
        }
    }
}
