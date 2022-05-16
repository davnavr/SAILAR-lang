namespace SAILARSharp.Reader {
    using System;

    public sealed class InvalidRecordTypeException : Exception {
        private readonly RecordType recordType;

        public InvalidRecordTypeException(RecordType recordType) : base(recordType + " is not a valid record type value") {
            this.recordType = recordType;
        }

        public RecordType RecordType => recordType;
    }
}
