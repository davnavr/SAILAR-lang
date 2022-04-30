namespace SAILSharp.Interop {
    using System;
    using System.Text;

    public unsafe readonly ref struct Identifier {
        readonly SAILAR.OpaqueIdentifier* identifier;

        public Identifier(string contents) {
            byte[] bytes = Encoding.UTF8.GetBytes(contents);
            SAILAR.OpaqueError* error = null;
            fixed (byte* b = bytes) {
                identifier = SAILAR.CreateIdentifier(b, (UIntPtr)bytes.Length, in error);
            }

            if (error != null) {
                throw new NotImplementedException("a");
            }
        }

        public void Dispose() {
            SAILAR.DisposeIdentifier(identifier);
        }
    }
}
