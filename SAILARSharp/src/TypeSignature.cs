namespace SAILARSharp {
    using System;
    using SAILARSharp.Interop;

    public unsafe sealed class TypeSignature : IDisposable {
        private OpaqueTypeSignature* signature;
        private TypeSignatureKind? kind = null;
        private bool disposed = false;
        private readonly object theLock = new();

        internal TypeSignature(OpaqueTypeSignature* signature) {
            this.signature = signature;
        }

        public OpaqueTypeSignature* Signature {
            get {
                if (disposed) {
                    throw new ObjectDisposedException(null);
                }

                return signature;
            }
        }

        public TypeSignatureKind Kind {
            get {
                lock (theLock) {
                    if (!kind.HasValue) {
                        kind = SAILAR.GetTypeSignatureKind(signature);
                    }

                    return kind.Value;
                }
            }
        }

        public void Dispose() {
            lock (theLock) {
                if (!disposed) {
                    GC.SuppressFinalize(this);
                    SAILAR.DisposeTypeSignature(signature);
                    signature = null;
                    disposed = true;
                }
            }
        }

        ~TypeSignature() {
            Dispose();
        }
    }
}
