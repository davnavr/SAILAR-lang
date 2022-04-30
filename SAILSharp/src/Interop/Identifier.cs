namespace SAILSharp.Interop {
    using System;
    using System.Text;

    public unsafe readonly ref struct Identifier {
        internal readonly SAILAR.OpaqueIdentifier* identifier;

        public Identifier(string contents) {
            byte[] bytes = Encoding.UTF8.GetBytes(contents);
            SAILAR.OpaqueError* error = null;
            fixed (byte* b = bytes) {
                identifier = SAILAR.CreateIdentifier(b, (UIntPtr)bytes.Length, in error);
            }

            Error.HandleError(error);
        }

        public override string ToString() {
            var length = UIntPtr.Zero;
            var content = SAILAR.GetIdentifierContents(identifier, in length);
            return Encoding.UTF8.GetString(content, (int)length);
        }

        public void Dispose() {
            SAILAR.DisposeIdentifier(identifier);
        }
    }
}
