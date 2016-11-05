perdure
=======

Robust persistence for typed immutable data

Perdure(TM), a Cognimeta product, aims to provide a simple and robust persistence mechanism for acyclic immutable data with an easily comprehensible cost-model. It persists to file or to raw block devices.

It is currently available for Haskell and is ***in the process of being ported to Scala***.

For some classes of applications, it can replace the use of traditional DMBS and keep the data modelled in a manner that is natural for purely functional languages. It offers nearly orthogonal persistence, minimizing the scope of changes required to make an application persistent.

The persistence process is strict, it does not persist thunks or closures, but only the fully evaluated data, which must be acyclic. It also requires some changes to the data types and algorithms. References must be inserted at some strategic places within data structures. These cut the structure into separately loadable parts. These parts should hold at least a few hundred bytes to reduce the frequency of disk reads. The application has control over the representation so it can optimize it in terms of the anticipated access patterns. A SizeRef reference type is provided which separates automatically if its size threshold is reached, and inlines the data otherwise. As a convenience, a Map type is also provided which uses SizeRef on internal tree nodes. It can grow beyond the memory size and still remain efficient for lookups and traversals. Other persistent data structures with similar properties can be built on top of Perdure.

For the user application, dereferencing is a pure computation. This is similar to lazy loading, but the reference data types do not hold ordinary references to their referent, only their location. This allows the referent to be unloaded transparently. Also since the reference holds the location and size of the data on the storage medium, dereferencing is very simple and efficient. No index need to be accessed, we only need to check a cache in case the data is already loaded.

Persisters for data types are created using safe combinators. Serialization is done at the bit level, allowing for very compact representations.

Undetected corruption is virtually impossible even in the presence of drive failures. Each separately loadable section has a 128-bit digest, and that digest is stored in the reference(s). This approach alleviates the need for low-level storage replication such as RAID, and takes care of replication at the persistence layer level. This seems appropriate given the increasing need for geographically distributed replication.

Persistence occurs in discrete transactions. These run in the IO monad. Transactions may be requested by multiple threads but they may block until they get their turn. Within a transaction, multiple threads or sparks can be used to examine the current state and compute the next state to be persisted.

Reference counting is used to automatically reclaim unused storage.

Since persisted data is immutable, it is trivial for applications to keep some or all historical states by using a list-like type as their root persisted type. Those past states can be used for analysis or possibly to recover data lost because of an application-level error. The library includes a History data type which automatically preserves a greater number of recent states and fewer older states.

We support 32bit, 64bit, little-endian and big-endian architectures. We allow platforms to write data and generate digests in their native format for maximum speed, but they should be able to read the data written by other platforms with the necessary conversions. Each reference stores the endianness and word size of the referent representation so databases can be moved between platforms without any conversion, and they could be read concurrently by different platforms.

A general mechanism Database.Perdure.Rev is provided to upgrade the persisted types. Its goal is similar to that of the safecopy package. Here however, the Rev module simply exports a type that is similar to Either but whose persister leaves room in the representation to accommodate future versions. We use it to create growing lists of alternatives such as User1.User :> User0.User :> NoRev. This type should always be your root persisted type. You should also use it on the nested parts of your data whose type is more subject to change: this will avoid having to propagate all type changes up to the root.

This library is young and there remains limitations that will need to be addressed in future releases:

* Memory references are not tracked. At this point it is assumed that the current persisted state is the only root for persistent GC purposes. So no reference must be kept to previous persisted states. This is not enforced by the API so care is needed. Otherwise dereferencing a dangling reference might fail (but will not return bad data thanks to the digest).

* Care is needed by the application developers to ensure they do not change type persisters. The persisters themselves are not persisted so the library cannot verify that it is using the same persister as with previous executions. There are two problems with persisting persisters: they are not acyclic, and they refer to user specified bijections. We encourage you to treat types and persisters as immutable code. Put each persisted type in a distinct file, such as User0.hs. When a revision is needed create User1.hs.

* The current allocation strategy is trivial. We must add a new node type in the free space representation that will allow us to search efficiently for a block of sufficient size. We currently scan all free intervals from the start. This relatively simple fix should be done shortly, but until it is done the library will not be appropriate for databases beyond ~100MB.

* Reclamation of free space simply occurs after some fixed number of state writes.

* Replication is local only at the moment. We write to all replicas, and read from the first correct one. We need to support remote storage which can become temporarily unavailable, and user controlled policies regarding those occurrences. Note that quorum voting is not needed for replica control of most (non-root) allocations because digests are contained in the references and we can detect when we are reading outdated data.

* Only raw disks configured for write-through caching will guarantee atomicity. Any other setup introduces more layers, such as file systems, which could lie about when data has actually been written out to disk. Perhaps our view is too pessimistic, and with proper action by this library other storage types could be supported. Currently file based storage is only supported for testing purposes and should not be used for live databases.

* A directory of bad sectors with alternative locations will need to be added. It will only need to be looked-up and updated for reads with incorrect digests.

* Sharing is not realized unless it happens in separate steps. During the writing of some state, a reference creates an allocation. In subsequent states we can refer to that same reference in multiple locations and sharing will occur. If a reference is shared in memory before being first written out, it will be written out multiple times without sharing of the referent. In the future we may rely on System.Mem.StableName to improve on that.

* Currently, references are local. We would like to be able to refer to the allocations of another thread or process. This will be a more involved addition so it will require distributed garbage collection such as weighted reference counting.

* Documentation and examples are not sufficient and should be our first focus. For a minimal example, look at Database.Perdure.TestState.testStatesF in exe-src.

Given those limitations, Perdure is not applicable for very large scale projects at the moment. But it can be ideal for smaller projects where there is no point in burdening the developer with a distinct data model. It can also be used as a temporary solution before integrating to external databases.

Cognimeta's Iota Charts web application https://iotacharts.com, is based on Perdure and has been has been live since 2011. Its database is relatively small at ~80MB currently, but we have been very pleased with the results.

The persistence mechanism is relatively simple and concise and its open source nature can provide the inquisitive user with added confidence about the security of its data. 
