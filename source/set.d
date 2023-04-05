/++
Static interfaces, helpers and some implementations for sets.
+/
module eris.set;


version (D_Ddoc) {
	interface IntensionalSet(Element) {
		bool contains(in Element x) const;
	}
}

enum bool isIntensionalSet(Set, Element) = __traits(compiles, (const(Set) s){
	bool b = s.contains(Element.init);
});

bool contains(Callable, Element)(in Callable predicate, in Element x) {
	return predicate(x);
}

static assert(isIntensionalSet!(bool delegate(int), int));


version (D_Ddoc) {
	interface ExtensionalSet(Element) : IntensionalSet!Element {
		size_t length() const;

		inout(Element)* get(in Element x) inout;

		int opApply(scope int delegate(ref Element) dg);
		int opApply(scope int delegate(ref const(Element)) dg) const;
	}
}

enum bool isExtensionalSet(Set, Element) = isIntensionalSet!(Set, Element) && __traits(compiles, (inout(Set) s){
	size_t size = s.length;
	foreach (inout(Element) x; s) {
		inout(Element)* p = s.get(x);
	}
});

inout(Element) get(Set, Element)(ref inout(Set) self, in Element x, scope Element delegate() makeDefault)
if (isExtensionalSet!(Set, Element))
{
	inout(Element)* entry = self.get(x);
	return entry ? *entry : makeDefault();
}


version (D_Ddoc) {
	interface MutableSet(Element) : ExtensionalSet!Element {
		Element* upsert(in Element x, scope Element delegate() create, scope void delegate(ref Element) update);

		void remove(in Element x, scope void delegate(ref Element) destroy);

		void clear(scope void delegate(ref Element) destroy);

		size_t reserve(size_t capacity);

		size_t capacity() const;
	}
}

enum bool isMutableSet(Set, Element) = isExtensionalSet!(Set, Element) && __traits(compiles, (Set s, Element x){
	const(Set) c = s;
	size_t reserved = c.capacity;
	size_t newCapacity = s.reserve(reserved * 2);
	Element* p = s.upsert(x, () => x, (ref old){ old = x; });
	auto d = (ref Element e){ .destroy(e); };
	s.remove(x, d);
	s.clear(d);
});

Element* upsert(Set, Element)(ref Set self, Element x)
if (isMutableSet!(Set, Element))
{
	return self.upsert(x, () => x, (ref old){ old = x; });
}

bool remove(Set, Element)(ref Set self, in Element x)
if (isMutableSet!(Set, Element))
{
	bool wasPresent = false;
	self.remove(x, (ref removed){ .destroy(removed); wasPresent = true; });
	return wasPresent;
}

void clear(Set, Element)(ref Set self)
if (isMutableSet!(Set, Element))
{
	self.clear((ref x){ .destroy(x); });
}

Element* require(Set, Element)(ref Set self, in Element x, scope Element delegate() create)
if (isMutableSet!(Set, Element))
{
	return self.upsert(x, create, (ref unchanged){});
}

unittest {
	assert(1 + 1 == 2);
	assert(false, "FIXME: just testing unittests");
}
