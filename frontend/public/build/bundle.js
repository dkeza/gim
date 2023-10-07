
(function(l, r) { if (!l || l.getElementById('livereloadscript')) return; r = l.createElement('script'); r.async = 1; r.src = '//' + (self.location.host || 'localhost').split(':')[0] + ':35729/livereload.js?snipver=1'; r.id = 'livereloadscript'; l.getElementsByTagName('head')[0].appendChild(r) })(self.document);
var app = (function () {
    'use strict';

    function noop() { }
    function assign(tar, src) {
        // @ts-ignore
        for (const k in src)
            tar[k] = src[k];
        return tar;
    }
    function add_location(element, file, line, column, char) {
        element.__svelte_meta = {
            loc: { file, line, column, char }
        };
    }
    function run(fn) {
        return fn();
    }
    function blank_object() {
        return Object.create(null);
    }
    function run_all(fns) {
        fns.forEach(run);
    }
    function is_function(thing) {
        return typeof thing === 'function';
    }
    function safe_not_equal(a, b) {
        return a != a ? b == b : a !== b || ((a && typeof a === 'object') || typeof a === 'function');
    }
    let src_url_equal_anchor;
    function src_url_equal(element_src, url) {
        if (!src_url_equal_anchor) {
            src_url_equal_anchor = document.createElement('a');
        }
        src_url_equal_anchor.href = url;
        return element_src === src_url_equal_anchor.href;
    }
    function is_empty(obj) {
        return Object.keys(obj).length === 0;
    }
    function validate_store(store, name) {
        if (store != null && typeof store.subscribe !== 'function') {
            throw new Error(`'${name}' is not a store with a 'subscribe' method`);
        }
    }
    function subscribe(store, ...callbacks) {
        if (store == null) {
            return noop;
        }
        const unsub = store.subscribe(...callbacks);
        return unsub.unsubscribe ? () => unsub.unsubscribe() : unsub;
    }
    function component_subscribe(component, store, callback) {
        component.$$.on_destroy.push(subscribe(store, callback));
    }
    function create_slot(definition, ctx, $$scope, fn) {
        if (definition) {
            const slot_ctx = get_slot_context(definition, ctx, $$scope, fn);
            return definition[0](slot_ctx);
        }
    }
    function get_slot_context(definition, ctx, $$scope, fn) {
        return definition[1] && fn
            ? assign($$scope.ctx.slice(), definition[1](fn(ctx)))
            : $$scope.ctx;
    }
    function get_slot_changes(definition, $$scope, dirty, fn) {
        if (definition[2] && fn) {
            const lets = definition[2](fn(dirty));
            if ($$scope.dirty === undefined) {
                return lets;
            }
            if (typeof lets === 'object') {
                const merged = [];
                const len = Math.max($$scope.dirty.length, lets.length);
                for (let i = 0; i < len; i += 1) {
                    merged[i] = $$scope.dirty[i] | lets[i];
                }
                return merged;
            }
            return $$scope.dirty | lets;
        }
        return $$scope.dirty;
    }
    function update_slot_base(slot, slot_definition, ctx, $$scope, slot_changes, get_slot_context_fn) {
        if (slot_changes) {
            const slot_context = get_slot_context(slot_definition, ctx, $$scope, get_slot_context_fn);
            slot.p(slot_context, slot_changes);
        }
    }
    function get_all_dirty_from_scope($$scope) {
        if ($$scope.ctx.length > 32) {
            const dirty = [];
            const length = $$scope.ctx.length / 32;
            for (let i = 0; i < length; i++) {
                dirty[i] = -1;
            }
            return dirty;
        }
        return -1;
    }
    function exclude_internal_props(props) {
        const result = {};
        for (const k in props)
            if (k[0] !== '$')
                result[k] = props[k];
        return result;
    }
    function compute_rest_props(props, keys) {
        const rest = {};
        keys = new Set(keys);
        for (const k in props)
            if (!keys.has(k) && k[0] !== '$')
                rest[k] = props[k];
        return rest;
    }
    function compute_slots(slots) {
        const result = {};
        for (const key in slots) {
            result[key] = true;
        }
        return result;
    }
    function set_store_value(store, ret, value) {
        store.set(value);
        return ret;
    }
    function action_destroyer(action_result) {
        return action_result && is_function(action_result.destroy) ? action_result.destroy : noop;
    }
    const contenteditable_truthy_values = ['', true, 1, 'true', 'contenteditable'];

    const globals = (typeof window !== 'undefined'
        ? window
        : typeof globalThis !== 'undefined'
            ? globalThis
            : global);
    function append(target, node) {
        target.appendChild(node);
    }
    function insert(target, node, anchor) {
        target.insertBefore(node, anchor || null);
    }
    function detach(node) {
        if (node.parentNode) {
            node.parentNode.removeChild(node);
        }
    }
    function element(name) {
        return document.createElement(name);
    }
    function svg_element(name) {
        return document.createElementNS('http://www.w3.org/2000/svg', name);
    }
    function text(data) {
        return document.createTextNode(data);
    }
    function space() {
        return text(' ');
    }
    function empty() {
        return text('');
    }
    function listen(node, event, handler, options) {
        node.addEventListener(event, handler, options);
        return () => node.removeEventListener(event, handler, options);
    }
    function prevent_default(fn) {
        return function (event) {
            event.preventDefault();
            // @ts-ignore
            return fn.call(this, event);
        };
    }
    function attr(node, attribute, value) {
        if (value == null)
            node.removeAttribute(attribute);
        else if (node.getAttribute(attribute) !== value)
            node.setAttribute(attribute, value);
    }
    /**
     * List of attributes that should always be set through the attr method,
     * because updating them through the property setter doesn't work reliably.
     * In the example of `width`/`height`, the problem is that the setter only
     * accepts numeric values, but the attribute can also be set to a string like `50%`.
     * If this list becomes too big, rethink this approach.
     */
    const always_set_through_set_attribute = ['width', 'height'];
    function set_attributes(node, attributes) {
        // @ts-ignore
        const descriptors = Object.getOwnPropertyDescriptors(node.__proto__);
        for (const key in attributes) {
            if (attributes[key] == null) {
                node.removeAttribute(key);
            }
            else if (key === 'style') {
                node.style.cssText = attributes[key];
            }
            else if (key === '__value') {
                node.value = node[key] = attributes[key];
            }
            else if (descriptors[key] && descriptors[key].set && always_set_through_set_attribute.indexOf(key) === -1) {
                node[key] = attributes[key];
            }
            else {
                attr(node, key, attributes[key]);
            }
        }
    }
    function set_svg_attributes(node, attributes) {
        for (const key in attributes) {
            attr(node, key, attributes[key]);
        }
    }
    function xlink_attr(node, attribute, value) {
        node.setAttributeNS('http://www.w3.org/1999/xlink', attribute, value);
    }
    function children(element) {
        return Array.from(element.childNodes);
    }
    function set_input_value(input, value) {
        input.value = value == null ? '' : value;
    }
    function toggle_class(element, name, toggle) {
        element.classList[toggle ? 'add' : 'remove'](name);
    }
    function custom_event(type, detail, { bubbles = false, cancelable = false } = {}) {
        const e = document.createEvent('CustomEvent');
        e.initCustomEvent(type, bubbles, cancelable, detail);
        return e;
    }

    let current_component;
    function set_current_component(component) {
        current_component = component;
    }
    function get_current_component() {
        if (!current_component)
            throw new Error('Function called outside component initialization');
        return current_component;
    }
    /**
     * Schedules a callback to run immediately after the component has been updated.
     *
     * The first time the callback runs will be after the initial `onMount`
     */
    function afterUpdate(fn) {
        get_current_component().$$.after_update.push(fn);
    }
    /**
     * Schedules a callback to run immediately before the component is unmounted.
     *
     * Out of `onMount`, `beforeUpdate`, `afterUpdate` and `onDestroy`, this is the
     * only one that runs inside a server-side component.
     *
     * https://svelte.dev/docs#run-time-svelte-ondestroy
     */
    function onDestroy(fn) {
        get_current_component().$$.on_destroy.push(fn);
    }
    /**
     * Creates an event dispatcher that can be used to dispatch [component events](/docs#template-syntax-component-directives-on-eventname).
     * Event dispatchers are functions that can take two arguments: `name` and `detail`.
     *
     * Component events created with `createEventDispatcher` create a
     * [CustomEvent](https://developer.mozilla.org/en-US/docs/Web/API/CustomEvent).
     * These events do not [bubble](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Building_blocks/Events#Event_bubbling_and_capture).
     * The `detail` argument corresponds to the [CustomEvent.detail](https://developer.mozilla.org/en-US/docs/Web/API/CustomEvent/detail)
     * property and can contain any type of data.
     *
     * https://svelte.dev/docs#run-time-svelte-createeventdispatcher
     */
    function createEventDispatcher() {
        const component = get_current_component();
        return (type, detail, { cancelable = false } = {}) => {
            const callbacks = component.$$.callbacks[type];
            if (callbacks) {
                // TODO are there situations where events could be dispatched
                // in a server (non-DOM) environment?
                const event = custom_event(type, detail, { cancelable });
                callbacks.slice().forEach(fn => {
                    fn.call(component, event);
                });
                return !event.defaultPrevented;
            }
            return true;
        };
    }
    // TODO figure out if we still want to support
    // shorthand events, or if we want to implement
    // a real bubbling mechanism
    function bubble(component, event) {
        const callbacks = component.$$.callbacks[event.type];
        if (callbacks) {
            // @ts-ignore
            callbacks.slice().forEach(fn => fn.call(this, event));
        }
    }

    const dirty_components = [];
    const binding_callbacks = [];
    let render_callbacks = [];
    const flush_callbacks = [];
    const resolved_promise = /* @__PURE__ */ Promise.resolve();
    let update_scheduled = false;
    function schedule_update() {
        if (!update_scheduled) {
            update_scheduled = true;
            resolved_promise.then(flush$1);
        }
    }
    function tick() {
        schedule_update();
        return resolved_promise;
    }
    function add_render_callback(fn) {
        render_callbacks.push(fn);
    }
    // flush() calls callbacks in this order:
    // 1. All beforeUpdate callbacks, in order: parents before children
    // 2. All bind:this callbacks, in reverse order: children before parents.
    // 3. All afterUpdate callbacks, in order: parents before children. EXCEPT
    //    for afterUpdates called during the initial onMount, which are called in
    //    reverse order: children before parents.
    // Since callbacks might update component values, which could trigger another
    // call to flush(), the following steps guard against this:
    // 1. During beforeUpdate, any updated components will be added to the
    //    dirty_components array and will cause a reentrant call to flush(). Because
    //    the flush index is kept outside the function, the reentrant call will pick
    //    up where the earlier call left off and go through all dirty components. The
    //    current_component value is saved and restored so that the reentrant call will
    //    not interfere with the "parent" flush() call.
    // 2. bind:this callbacks cannot trigger new flush() calls.
    // 3. During afterUpdate, any updated components will NOT have their afterUpdate
    //    callback called a second time; the seen_callbacks set, outside the flush()
    //    function, guarantees this behavior.
    const seen_callbacks = new Set();
    let flushidx = 0; // Do *not* move this inside the flush() function
    function flush$1() {
        // Do not reenter flush while dirty components are updated, as this can
        // result in an infinite loop. Instead, let the inner flush handle it.
        // Reentrancy is ok afterwards for bindings etc.
        if (flushidx !== 0) {
            return;
        }
        const saved_component = current_component;
        do {
            // first, call beforeUpdate functions
            // and update components
            try {
                while (flushidx < dirty_components.length) {
                    const component = dirty_components[flushidx];
                    flushidx++;
                    set_current_component(component);
                    update(component.$$);
                }
            }
            catch (e) {
                // reset dirty state to not end up in a deadlocked state and then rethrow
                dirty_components.length = 0;
                flushidx = 0;
                throw e;
            }
            set_current_component(null);
            dirty_components.length = 0;
            flushidx = 0;
            while (binding_callbacks.length)
                binding_callbacks.pop()();
            // then, once components are updated, call
            // afterUpdate functions. This may cause
            // subsequent updates...
            for (let i = 0; i < render_callbacks.length; i += 1) {
                const callback = render_callbacks[i];
                if (!seen_callbacks.has(callback)) {
                    // ...so guard against infinite loops
                    seen_callbacks.add(callback);
                    callback();
                }
            }
            render_callbacks.length = 0;
        } while (dirty_components.length);
        while (flush_callbacks.length) {
            flush_callbacks.pop()();
        }
        update_scheduled = false;
        seen_callbacks.clear();
        set_current_component(saved_component);
    }
    function update($$) {
        if ($$.fragment !== null) {
            $$.update();
            run_all($$.before_update);
            const dirty = $$.dirty;
            $$.dirty = [-1];
            $$.fragment && $$.fragment.p($$.ctx, dirty);
            $$.after_update.forEach(add_render_callback);
        }
    }
    /**
     * Useful for example to execute remaining `afterUpdate` callbacks before executing `destroy`.
     */
    function flush_render_callbacks(fns) {
        const filtered = [];
        const targets = [];
        render_callbacks.forEach((c) => fns.indexOf(c) === -1 ? filtered.push(c) : targets.push(c));
        targets.forEach((c) => c());
        render_callbacks = filtered;
    }
    const outroing = new Set();
    let outros;
    function group_outros() {
        outros = {
            r: 0,
            c: [],
            p: outros // parent group
        };
    }
    function check_outros() {
        if (!outros.r) {
            run_all(outros.c);
        }
        outros = outros.p;
    }
    function transition_in(block, local) {
        if (block && block.i) {
            outroing.delete(block);
            block.i(local);
        }
    }
    function transition_out(block, local, detach, callback) {
        if (block && block.o) {
            if (outroing.has(block))
                return;
            outroing.add(block);
            outros.c.push(() => {
                outroing.delete(block);
                if (callback) {
                    if (detach)
                        block.d(1);
                    callback();
                }
            });
            block.o(local);
        }
        else if (callback) {
            callback();
        }
    }

    function get_spread_update(levels, updates) {
        const update = {};
        const to_null_out = {};
        const accounted_for = { $$scope: 1 };
        let i = levels.length;
        while (i--) {
            const o = levels[i];
            const n = updates[i];
            if (n) {
                for (const key in o) {
                    if (!(key in n))
                        to_null_out[key] = 1;
                }
                for (const key in n) {
                    if (!accounted_for[key]) {
                        update[key] = n[key];
                        accounted_for[key] = 1;
                    }
                }
                levels[i] = n;
            }
            else {
                for (const key in o) {
                    accounted_for[key] = 1;
                }
            }
        }
        for (const key in to_null_out) {
            if (!(key in update))
                update[key] = undefined;
        }
        return update;
    }
    function get_spread_object(spread_props) {
        return typeof spread_props === 'object' && spread_props !== null ? spread_props : {};
    }
    function create_component(block) {
        block && block.c();
    }
    function mount_component(component, target, anchor, customElement) {
        const { fragment, after_update } = component.$$;
        fragment && fragment.m(target, anchor);
        if (!customElement) {
            // onMount happens before the initial afterUpdate
            add_render_callback(() => {
                const new_on_destroy = component.$$.on_mount.map(run).filter(is_function);
                // if the component was destroyed immediately
                // it will update the `$$.on_destroy` reference to `null`.
                // the destructured on_destroy may still reference to the old array
                if (component.$$.on_destroy) {
                    component.$$.on_destroy.push(...new_on_destroy);
                }
                else {
                    // Edge case - component was destroyed immediately,
                    // most likely as a result of a binding initialising
                    run_all(new_on_destroy);
                }
                component.$$.on_mount = [];
            });
        }
        after_update.forEach(add_render_callback);
    }
    function destroy_component(component, detaching) {
        const $$ = component.$$;
        if ($$.fragment !== null) {
            flush_render_callbacks($$.after_update);
            run_all($$.on_destroy);
            $$.fragment && $$.fragment.d(detaching);
            // TODO null out other refs, including component.$$ (but need to
            // preserve final state?)
            $$.on_destroy = $$.fragment = null;
            $$.ctx = [];
        }
    }
    function make_dirty(component, i) {
        if (component.$$.dirty[0] === -1) {
            dirty_components.push(component);
            schedule_update();
            component.$$.dirty.fill(0);
        }
        component.$$.dirty[(i / 31) | 0] |= (1 << (i % 31));
    }
    function init(component, options, instance, create_fragment, not_equal, props, append_styles, dirty = [-1]) {
        const parent_component = current_component;
        set_current_component(component);
        const $$ = component.$$ = {
            fragment: null,
            ctx: [],
            // state
            props,
            update: noop,
            not_equal,
            bound: blank_object(),
            // lifecycle
            on_mount: [],
            on_destroy: [],
            on_disconnect: [],
            before_update: [],
            after_update: [],
            context: new Map(options.context || (parent_component ? parent_component.$$.context : [])),
            // everything else
            callbacks: blank_object(),
            dirty,
            skip_bound: false,
            root: options.target || parent_component.$$.root
        };
        append_styles && append_styles($$.root);
        let ready = false;
        $$.ctx = instance
            ? instance(component, options.props || {}, (i, ret, ...rest) => {
                const value = rest.length ? rest[0] : ret;
                if ($$.ctx && not_equal($$.ctx[i], $$.ctx[i] = value)) {
                    if (!$$.skip_bound && $$.bound[i])
                        $$.bound[i](value);
                    if (ready)
                        make_dirty(component, i);
                }
                return ret;
            })
            : [];
        $$.update();
        ready = true;
        run_all($$.before_update);
        // `false` as a special case of no DOM component
        $$.fragment = create_fragment ? create_fragment($$.ctx) : false;
        if (options.target) {
            if (options.hydrate) {
                const nodes = children(options.target);
                // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                $$.fragment && $$.fragment.l(nodes);
                nodes.forEach(detach);
            }
            else {
                // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                $$.fragment && $$.fragment.c();
            }
            if (options.intro)
                transition_in(component.$$.fragment);
            mount_component(component, options.target, options.anchor, options.customElement);
            flush$1();
        }
        set_current_component(parent_component);
    }
    /**
     * Base class for Svelte components. Used when dev=false.
     */
    class SvelteComponent {
        $destroy() {
            destroy_component(this, 1);
            this.$destroy = noop;
        }
        $on(type, callback) {
            if (!is_function(callback)) {
                return noop;
            }
            const callbacks = (this.$$.callbacks[type] || (this.$$.callbacks[type] = []));
            callbacks.push(callback);
            return () => {
                const index = callbacks.indexOf(callback);
                if (index !== -1)
                    callbacks.splice(index, 1);
            };
        }
        $set($$props) {
            if (this.$$set && !is_empty($$props)) {
                this.$$.skip_bound = true;
                this.$$set($$props);
                this.$$.skip_bound = false;
            }
        }
    }

    function dispatch_dev(type, detail) {
        document.dispatchEvent(custom_event(type, Object.assign({ version: '3.59.2' }, detail), { bubbles: true }));
    }
    function append_dev(target, node) {
        dispatch_dev('SvelteDOMInsert', { target, node });
        append(target, node);
    }
    function insert_dev(target, node, anchor) {
        dispatch_dev('SvelteDOMInsert', { target, node, anchor });
        insert(target, node, anchor);
    }
    function detach_dev(node) {
        dispatch_dev('SvelteDOMRemove', { node });
        detach(node);
    }
    function listen_dev(node, event, handler, options, has_prevent_default, has_stop_propagation, has_stop_immediate_propagation) {
        const modifiers = options === true ? ['capture'] : options ? Array.from(Object.keys(options)) : [];
        if (has_prevent_default)
            modifiers.push('preventDefault');
        if (has_stop_propagation)
            modifiers.push('stopPropagation');
        if (has_stop_immediate_propagation)
            modifiers.push('stopImmediatePropagation');
        dispatch_dev('SvelteDOMAddEventListener', { node, event, handler, modifiers });
        const dispose = listen(node, event, handler, options);
        return () => {
            dispatch_dev('SvelteDOMRemoveEventListener', { node, event, handler, modifiers });
            dispose();
        };
    }
    function attr_dev(node, attribute, value) {
        attr(node, attribute, value);
        if (value == null)
            dispatch_dev('SvelteDOMRemoveAttribute', { node, attribute });
        else
            dispatch_dev('SvelteDOMSetAttribute', { node, attribute, value });
    }
    function set_data_dev(text, data) {
        data = '' + data;
        if (text.data === data)
            return;
        dispatch_dev('SvelteDOMSetData', { node: text, data });
        text.data = data;
    }
    function set_data_contenteditable_dev(text, data) {
        data = '' + data;
        if (text.wholeText === data)
            return;
        dispatch_dev('SvelteDOMSetData', { node: text, data });
        text.data = data;
    }
    function set_data_maybe_contenteditable_dev(text, data, attr_value) {
        if (~contenteditable_truthy_values.indexOf(attr_value)) {
            set_data_contenteditable_dev(text, data);
        }
        else {
            set_data_dev(text, data);
        }
    }
    function validate_slots(name, slot, keys) {
        for (const slot_key of Object.keys(slot)) {
            if (!~keys.indexOf(slot_key)) {
                console.warn(`<${name}> received an unexpected slot "${slot_key}".`);
            }
        }
    }
    function construct_svelte_component_dev(component, props) {
        const error_message = 'this={...} of <svelte:component> should specify a Svelte component.';
        try {
            const instance = new component(props);
            if (!instance.$$ || !instance.$set || !instance.$on || !instance.$destroy) {
                throw new Error(error_message);
            }
            return instance;
        }
        catch (err) {
            const { message } = err;
            if (typeof message === 'string' && message.indexOf('is not a constructor') !== -1) {
                throw new Error(error_message);
            }
            else {
                throw err;
            }
        }
    }
    /**
     * Base class for Svelte components with some minor dev-enhancements. Used when dev=true.
     */
    class SvelteComponentDev extends SvelteComponent {
        constructor(options) {
            if (!options || (!options.target && !options.$$inline)) {
                throw new Error("'target' is a required option");
            }
            super();
        }
        $destroy() {
            super.$destroy();
            this.$destroy = () => {
                console.warn('Component was already destroyed'); // eslint-disable-line no-console
            };
        }
        $capture_state() { }
        $inject_state() { }
    }

    const subscriber_queue = [];
    /**
     * Creates a `Readable` store that allows reading by subscription.
     * @param value initial value
     * @param {StartStopNotifier} [start]
     */
    function readable(value, start) {
        return {
            subscribe: writable(value, start).subscribe
        };
    }
    /**
     * Create a `Writable` store that allows both updating and reading by subscription.
     * @param {*=}value initial value
     * @param {StartStopNotifier=} start
     */
    function writable(value, start = noop) {
        let stop;
        const subscribers = new Set();
        function set(new_value) {
            if (safe_not_equal(value, new_value)) {
                value = new_value;
                if (stop) { // store is ready
                    const run_queue = !subscriber_queue.length;
                    for (const subscriber of subscribers) {
                        subscriber[1]();
                        subscriber_queue.push(subscriber, value);
                    }
                    if (run_queue) {
                        for (let i = 0; i < subscriber_queue.length; i += 2) {
                            subscriber_queue[i][0](subscriber_queue[i + 1]);
                        }
                        subscriber_queue.length = 0;
                    }
                }
            }
        }
        function update(fn) {
            set(fn(value));
        }
        function subscribe(run, invalidate = noop) {
            const subscriber = [run, invalidate];
            subscribers.add(subscriber);
            if (subscribers.size === 1) {
                stop = start(set) || noop;
            }
            run(value);
            return () => {
                subscribers.delete(subscriber);
                if (subscribers.size === 0 && stop) {
                    stop();
                    stop = null;
                }
            };
        }
        return { set, update, subscribe };
    }
    function derived(stores, fn, initial_value) {
        const single = !Array.isArray(stores);
        const stores_array = single
            ? [stores]
            : stores;
        const auto = fn.length < 2;
        return readable(initial_value, (set) => {
            let started = false;
            const values = [];
            let pending = 0;
            let cleanup = noop;
            const sync = () => {
                if (pending) {
                    return;
                }
                cleanup();
                const result = fn(single ? values[0] : values, set);
                if (auto) {
                    set(result);
                }
                else {
                    cleanup = is_function(result) ? result : noop;
                }
            };
            const unsubscribers = stores_array.map((store, i) => subscribe(store, (value) => {
                values[i] = value;
                pending &= ~(1 << i);
                if (started) {
                    sync();
                }
            }, () => {
                pending |= (1 << i);
            }));
            started = true;
            sync();
            return function stop() {
                run_all(unsubscribers);
                cleanup();
                // We need to set this to false because callbacks can still happen despite having unsubscribed:
                // Callbacks might already be placed in the queue which doesn't know it should no longer
                // invoke this derived store.
                started = false;
            };
        });
    }

    const s = writable(
        {
            userId: '',
            userName: '',
            loggedIn: false
        },
    );

    /**
     * @typedef {Object} WrappedComponent Object returned by the `wrap` method
     * @property {SvelteComponent} component - Component to load (this is always asynchronous)
     * @property {RoutePrecondition[]} [conditions] - Route pre-conditions to validate
     * @property {Object} [props] - Optional dictionary of static props
     * @property {Object} [userData] - Optional user data dictionary
     * @property {bool} _sveltesparouter - Internal flag; always set to true
     */

    /**
     * @callback AsyncSvelteComponent
     * @returns {Promise<SvelteComponent>} Returns a Promise that resolves with a Svelte component
     */

    /**
     * @callback RoutePrecondition
     * @param {RouteDetail} detail - Route detail object
     * @returns {boolean|Promise<boolean>} If the callback returns a false-y value, it's interpreted as the precondition failed, so it aborts loading the component (and won't process other pre-condition callbacks)
     */

    /**
     * @typedef {Object} WrapOptions Options object for the call to `wrap`
     * @property {SvelteComponent} [component] - Svelte component to load (this is incompatible with `asyncComponent`)
     * @property {AsyncSvelteComponent} [asyncComponent] - Function that returns a Promise that fulfills with a Svelte component (e.g. `{asyncComponent: () => import('Foo.svelte')}`)
     * @property {SvelteComponent} [loadingComponent] - Svelte component to be displayed while the async route is loading (as a placeholder); when unset or false-y, no component is shown while component
     * @property {object} [loadingParams] - Optional dictionary passed to the `loadingComponent` component as params (for an exported prop called `params`)
     * @property {object} [userData] - Optional object that will be passed to events such as `routeLoading`, `routeLoaded`, `conditionsFailed`
     * @property {object} [props] - Optional key-value dictionary of static props that will be passed to the component. The props are expanded with {...props}, so the key in the dictionary becomes the name of the prop.
     * @property {RoutePrecondition[]|RoutePrecondition} [conditions] - Route pre-conditions to add, which will be executed in order
     */

    /**
     * Wraps a component to enable multiple capabilities:
     * 1. Using dynamically-imported component, with (e.g. `{asyncComponent: () => import('Foo.svelte')}`), which also allows bundlers to do code-splitting.
     * 2. Adding route pre-conditions (e.g. `{conditions: [...]}`)
     * 3. Adding static props that are passed to the component
     * 4. Adding custom userData, which is passed to route events (e.g. route loaded events) or to route pre-conditions (e.g. `{userData: {foo: 'bar}}`)
     * 
     * @param {WrapOptions} args - Arguments object
     * @returns {WrappedComponent} Wrapped component
     */
    function wrap$1(args) {
        if (!args) {
            throw Error('Parameter args is required')
        }

        // We need to have one and only one of component and asyncComponent
        // This does a "XNOR"
        if (!args.component == !args.asyncComponent) {
            throw Error('One and only one of component and asyncComponent is required')
        }

        // If the component is not async, wrap it into a function returning a Promise
        if (args.component) {
            args.asyncComponent = () => Promise.resolve(args.component);
        }

        // Parameter asyncComponent and each item of conditions must be functions
        if (typeof args.asyncComponent != 'function') {
            throw Error('Parameter asyncComponent must be a function')
        }
        if (args.conditions) {
            // Ensure it's an array
            if (!Array.isArray(args.conditions)) {
                args.conditions = [args.conditions];
            }
            for (let i = 0; i < args.conditions.length; i++) {
                if (!args.conditions[i] || typeof args.conditions[i] != 'function') {
                    throw Error('Invalid parameter conditions[' + i + ']')
                }
            }
        }

        // Check if we have a placeholder component
        if (args.loadingComponent) {
            args.asyncComponent.loading = args.loadingComponent;
            args.asyncComponent.loadingParams = args.loadingParams || undefined;
        }

        // Returns an object that contains all the functions to execute too
        // The _sveltesparouter flag is to confirm the object was created by this router
        const obj = {
            component: args.asyncComponent,
            userData: args.userData,
            conditions: (args.conditions && args.conditions.length) ? args.conditions : undefined,
            props: (args.props && Object.keys(args.props).length) ? args.props : {},
            _sveltesparouter: true
        };

        return obj
    }

    function parse$1(str, loose) {
    	if (str instanceof RegExp) return { keys:false, pattern:str };
    	var c, o, tmp, ext, keys=[], pattern='', arr = str.split('/');
    	arr[0] || arr.shift();

    	while (tmp = arr.shift()) {
    		c = tmp[0];
    		if (c === '*') {
    			keys.push('wild');
    			pattern += '/(.*)';
    		} else if (c === ':') {
    			o = tmp.indexOf('?', 1);
    			ext = tmp.indexOf('.', 1);
    			keys.push( tmp.substring(1, !!~o ? o : !!~ext ? ext : tmp.length) );
    			pattern += !!~o && !~ext ? '(?:/([^/]+?))?' : '/([^/]+?)';
    			if (!!~ext) pattern += (!!~o ? '?' : '') + '\\' + tmp.substring(ext);
    		} else {
    			pattern += '/' + tmp;
    		}
    	}

    	return {
    		keys: keys,
    		pattern: new RegExp('^' + pattern + (loose ? '(?=$|\/)' : '\/?$'), 'i')
    	};
    }

    /* node_modules\svelte-spa-router\Router.svelte generated by Svelte v3.59.2 */

    const { Error: Error_1, Object: Object_1, console: console_1$2 } = globals;

    // (267:0) {:else}
    function create_else_block$5(ctx) {
    	let switch_instance;
    	let switch_instance_anchor;
    	let current;
    	const switch_instance_spread_levels = [/*props*/ ctx[2]];
    	var switch_value = /*component*/ ctx[0];

    	function switch_props(ctx) {
    		let switch_instance_props = {};

    		for (let i = 0; i < switch_instance_spread_levels.length; i += 1) {
    			switch_instance_props = assign(switch_instance_props, switch_instance_spread_levels[i]);
    		}

    		return {
    			props: switch_instance_props,
    			$$inline: true
    		};
    	}

    	if (switch_value) {
    		switch_instance = construct_svelte_component_dev(switch_value, switch_props());
    		switch_instance.$on("routeEvent", /*routeEvent_handler_1*/ ctx[7]);
    	}

    	const block = {
    		c: function create() {
    			if (switch_instance) create_component(switch_instance.$$.fragment);
    			switch_instance_anchor = empty();
    		},
    		m: function mount(target, anchor) {
    			if (switch_instance) mount_component(switch_instance, target, anchor);
    			insert_dev(target, switch_instance_anchor, anchor);
    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			const switch_instance_changes = (dirty & /*props*/ 4)
    			? get_spread_update(switch_instance_spread_levels, [get_spread_object(/*props*/ ctx[2])])
    			: {};

    			if (dirty & /*component*/ 1 && switch_value !== (switch_value = /*component*/ ctx[0])) {
    				if (switch_instance) {
    					group_outros();
    					const old_component = switch_instance;

    					transition_out(old_component.$$.fragment, 1, 0, () => {
    						destroy_component(old_component, 1);
    					});

    					check_outros();
    				}

    				if (switch_value) {
    					switch_instance = construct_svelte_component_dev(switch_value, switch_props());
    					switch_instance.$on("routeEvent", /*routeEvent_handler_1*/ ctx[7]);
    					create_component(switch_instance.$$.fragment);
    					transition_in(switch_instance.$$.fragment, 1);
    					mount_component(switch_instance, switch_instance_anchor.parentNode, switch_instance_anchor);
    				} else {
    					switch_instance = null;
    				}
    			} else if (switch_value) {
    				switch_instance.$set(switch_instance_changes);
    			}
    		},
    		i: function intro(local) {
    			if (current) return;
    			if (switch_instance) transition_in(switch_instance.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			if (switch_instance) transition_out(switch_instance.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(switch_instance_anchor);
    			if (switch_instance) destroy_component(switch_instance, detaching);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_else_block$5.name,
    		type: "else",
    		source: "(267:0) {:else}",
    		ctx
    	});

    	return block;
    }

    // (260:0) {#if componentParams}
    function create_if_block$7(ctx) {
    	let switch_instance;
    	let switch_instance_anchor;
    	let current;
    	const switch_instance_spread_levels = [{ params: /*componentParams*/ ctx[1] }, /*props*/ ctx[2]];
    	var switch_value = /*component*/ ctx[0];

    	function switch_props(ctx) {
    		let switch_instance_props = {};

    		for (let i = 0; i < switch_instance_spread_levels.length; i += 1) {
    			switch_instance_props = assign(switch_instance_props, switch_instance_spread_levels[i]);
    		}

    		return {
    			props: switch_instance_props,
    			$$inline: true
    		};
    	}

    	if (switch_value) {
    		switch_instance = construct_svelte_component_dev(switch_value, switch_props());
    		switch_instance.$on("routeEvent", /*routeEvent_handler*/ ctx[6]);
    	}

    	const block = {
    		c: function create() {
    			if (switch_instance) create_component(switch_instance.$$.fragment);
    			switch_instance_anchor = empty();
    		},
    		m: function mount(target, anchor) {
    			if (switch_instance) mount_component(switch_instance, target, anchor);
    			insert_dev(target, switch_instance_anchor, anchor);
    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			const switch_instance_changes = (dirty & /*componentParams, props*/ 6)
    			? get_spread_update(switch_instance_spread_levels, [
    					dirty & /*componentParams*/ 2 && { params: /*componentParams*/ ctx[1] },
    					dirty & /*props*/ 4 && get_spread_object(/*props*/ ctx[2])
    				])
    			: {};

    			if (dirty & /*component*/ 1 && switch_value !== (switch_value = /*component*/ ctx[0])) {
    				if (switch_instance) {
    					group_outros();
    					const old_component = switch_instance;

    					transition_out(old_component.$$.fragment, 1, 0, () => {
    						destroy_component(old_component, 1);
    					});

    					check_outros();
    				}

    				if (switch_value) {
    					switch_instance = construct_svelte_component_dev(switch_value, switch_props());
    					switch_instance.$on("routeEvent", /*routeEvent_handler*/ ctx[6]);
    					create_component(switch_instance.$$.fragment);
    					transition_in(switch_instance.$$.fragment, 1);
    					mount_component(switch_instance, switch_instance_anchor.parentNode, switch_instance_anchor);
    				} else {
    					switch_instance = null;
    				}
    			} else if (switch_value) {
    				switch_instance.$set(switch_instance_changes);
    			}
    		},
    		i: function intro(local) {
    			if (current) return;
    			if (switch_instance) transition_in(switch_instance.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			if (switch_instance) transition_out(switch_instance.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(switch_instance_anchor);
    			if (switch_instance) destroy_component(switch_instance, detaching);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block$7.name,
    		type: "if",
    		source: "(260:0) {#if componentParams}",
    		ctx
    	});

    	return block;
    }

    function create_fragment$c(ctx) {
    	let current_block_type_index;
    	let if_block;
    	let if_block_anchor;
    	let current;
    	const if_block_creators = [create_if_block$7, create_else_block$5];
    	const if_blocks = [];

    	function select_block_type(ctx, dirty) {
    		if (/*componentParams*/ ctx[1]) return 0;
    		return 1;
    	}

    	current_block_type_index = select_block_type(ctx);
    	if_block = if_blocks[current_block_type_index] = if_block_creators[current_block_type_index](ctx);

    	const block = {
    		c: function create() {
    			if_block.c();
    			if_block_anchor = empty();
    		},
    		l: function claim(nodes) {
    			throw new Error_1("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			if_blocks[current_block_type_index].m(target, anchor);
    			insert_dev(target, if_block_anchor, anchor);
    			current = true;
    		},
    		p: function update(ctx, [dirty]) {
    			let previous_block_index = current_block_type_index;
    			current_block_type_index = select_block_type(ctx);

    			if (current_block_type_index === previous_block_index) {
    				if_blocks[current_block_type_index].p(ctx, dirty);
    			} else {
    				group_outros();

    				transition_out(if_blocks[previous_block_index], 1, 1, () => {
    					if_blocks[previous_block_index] = null;
    				});

    				check_outros();
    				if_block = if_blocks[current_block_type_index];

    				if (!if_block) {
    					if_block = if_blocks[current_block_type_index] = if_block_creators[current_block_type_index](ctx);
    					if_block.c();
    				} else {
    					if_block.p(ctx, dirty);
    				}

    				transition_in(if_block, 1);
    				if_block.m(if_block_anchor.parentNode, if_block_anchor);
    			}
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(if_block);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(if_block);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if_blocks[current_block_type_index].d(detaching);
    			if (detaching) detach_dev(if_block_anchor);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$c.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function wrap(component, userData, ...conditions) {
    	// Use the new wrap method and show a deprecation warning
    	// eslint-disable-next-line no-console
    	console.warn('Method `wrap` from `svelte-spa-router` is deprecated and will be removed in a future version. Please use `svelte-spa-router/wrap` instead. See http://bit.ly/svelte-spa-router-upgrading');

    	return wrap$1({ component, userData, conditions });
    }

    /**
     * @typedef {Object} Location
     * @property {string} location - Location (page/view), for example `/book`
     * @property {string} [querystring] - Querystring from the hash, as a string not parsed
     */
    /**
     * Returns the current location from the hash.
     *
     * @returns {Location} Location object
     * @private
     */
    function getLocation() {
    	const hashPosition = window.location.href.indexOf('#/');

    	let location = hashPosition > -1
    	? window.location.href.substr(hashPosition + 1)
    	: '/';

    	// Check if there's a querystring
    	const qsPosition = location.indexOf('?');

    	let querystring = '';

    	if (qsPosition > -1) {
    		querystring = location.substr(qsPosition + 1);
    		location = location.substr(0, qsPosition);
    	}

    	return { location, querystring };
    }

    const loc = readable(null, // eslint-disable-next-line prefer-arrow-callback
    function start(set) {
    	set(getLocation());

    	const update = () => {
    		set(getLocation());
    	};

    	window.addEventListener('hashchange', update, false);

    	return function stop() {
    		window.removeEventListener('hashchange', update, false);
    	};
    });

    const location = derived(loc, $loc => $loc.location);
    const querystring = derived(loc, $loc => $loc.querystring);
    const params = writable(undefined);

    async function push(location) {
    	if (!location || location.length < 1 || location.charAt(0) != '/' && location.indexOf('#/') !== 0) {
    		throw Error('Invalid parameter location');
    	}

    	// Execute this code when the current call stack is complete
    	await tick();

    	// Note: this will include scroll state in history even when restoreScrollState is false
    	history.replaceState(
    		{
    			...history.state,
    			__svelte_spa_router_scrollX: window.scrollX,
    			__svelte_spa_router_scrollY: window.scrollY
    		},
    		undefined
    	);

    	window.location.hash = (location.charAt(0) == '#' ? '' : '#') + location;
    }

    async function pop() {
    	// Execute this code when the current call stack is complete
    	await tick();

    	window.history.back();
    }

    async function replace(location) {
    	if (!location || location.length < 1 || location.charAt(0) != '/' && location.indexOf('#/') !== 0) {
    		throw Error('Invalid parameter location');
    	}

    	// Execute this code when the current call stack is complete
    	await tick();

    	const dest = (location.charAt(0) == '#' ? '' : '#') + location;

    	try {
    		const newState = { ...history.state };
    		delete newState['__svelte_spa_router_scrollX'];
    		delete newState['__svelte_spa_router_scrollY'];
    		window.history.replaceState(newState, undefined, dest);
    	} catch(e) {
    		// eslint-disable-next-line no-console
    		console.warn('Caught exception while replacing the current page. If you\'re running this in the Svelte REPL, please note that the `replace` method might not work in this environment.');
    	}

    	// The method above doesn't trigger the hashchange event, so let's do that manually
    	window.dispatchEvent(new Event('hashchange'));
    }

    function link(node, opts) {
    	opts = linkOpts(opts);

    	// Only apply to <a> tags
    	if (!node || !node.tagName || node.tagName.toLowerCase() != 'a') {
    		throw Error('Action "link" can only be used with <a> tags');
    	}

    	updateLink(node, opts);

    	return {
    		update(updated) {
    			updated = linkOpts(updated);
    			updateLink(node, updated);
    		}
    	};
    }

    function restoreScroll(state) {
    	// If this exists, then this is a back navigation: restore the scroll position
    	if (state) {
    		window.scrollTo(state.__svelte_spa_router_scrollX, state.__svelte_spa_router_scrollY);
    	} else {
    		// Otherwise this is a forward navigation: scroll to top
    		window.scrollTo(0, 0);
    	}
    }

    // Internal function used by the link function
    function updateLink(node, opts) {
    	let href = opts.href || node.getAttribute('href');

    	// Destination must start with '/' or '#/'
    	if (href && href.charAt(0) == '/') {
    		// Add # to the href attribute
    		href = '#' + href;
    	} else if (!href || href.length < 2 || href.slice(0, 2) != '#/') {
    		throw Error('Invalid value for "href" attribute: ' + href);
    	}

    	node.setAttribute('href', href);

    	node.addEventListener('click', event => {
    		// Prevent default anchor onclick behaviour
    		event.preventDefault();

    		if (!opts.disabled) {
    			scrollstateHistoryHandler(event.currentTarget.getAttribute('href'));
    		}
    	});
    }

    // Internal function that ensures the argument of the link action is always an object
    function linkOpts(val) {
    	if (val && typeof val == 'string') {
    		return { href: val };
    	} else {
    		return val || {};
    	}
    }

    /**
     * The handler attached to an anchor tag responsible for updating the
     * current history state with the current scroll state
     *
     * @param {string} href - Destination
     */
    function scrollstateHistoryHandler(href) {
    	// Setting the url (3rd arg) to href will break clicking for reasons, so don't try to do that
    	history.replaceState(
    		{
    			...history.state,
    			__svelte_spa_router_scrollX: window.scrollX,
    			__svelte_spa_router_scrollY: window.scrollY
    		},
    		undefined
    	);

    	// This will force an update as desired, but this time our scroll state will be attached
    	window.location.hash = href;
    }

    function instance$c($$self, $$props, $$invalidate) {
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Router', slots, []);
    	let { routes = {} } = $$props;
    	let { prefix = '' } = $$props;
    	let { restoreScrollState = false } = $$props;

    	/**
     * Container for a route: path, component
     */
    	class RouteItem {
    		/**
     * Initializes the object and creates a regular expression from the path, using regexparam.
     *
     * @param {string} path - Path to the route (must start with '/' or '*')
     * @param {SvelteComponent|WrappedComponent} component - Svelte component for the route, optionally wrapped
     */
    		constructor(path, component) {
    			if (!component || typeof component != 'function' && (typeof component != 'object' || component._sveltesparouter !== true)) {
    				throw Error('Invalid component object');
    			}

    			// Path must be a regular or expression, or a string starting with '/' or '*'
    			if (!path || typeof path == 'string' && (path.length < 1 || path.charAt(0) != '/' && path.charAt(0) != '*') || typeof path == 'object' && !(path instanceof RegExp)) {
    				throw Error('Invalid value for "path" argument - strings must start with / or *');
    			}

    			const { pattern, keys } = parse$1(path);
    			this.path = path;

    			// Check if the component is wrapped and we have conditions
    			if (typeof component == 'object' && component._sveltesparouter === true) {
    				this.component = component.component;
    				this.conditions = component.conditions || [];
    				this.userData = component.userData;
    				this.props = component.props || {};
    			} else {
    				// Convert the component to a function that returns a Promise, to normalize it
    				this.component = () => Promise.resolve(component);

    				this.conditions = [];
    				this.props = {};
    			}

    			this._pattern = pattern;
    			this._keys = keys;
    		}

    		/**
     * Checks if `path` matches the current route.
     * If there's a match, will return the list of parameters from the URL (if any).
     * In case of no match, the method will return `null`.
     *
     * @param {string} path - Path to test
     * @returns {null|Object.<string, string>} List of paramters from the URL if there's a match, or `null` otherwise.
     */
    		match(path) {
    			// If there's a prefix, check if it matches the start of the path.
    			// If not, bail early, else remove it before we run the matching.
    			if (prefix) {
    				if (typeof prefix == 'string') {
    					if (path.startsWith(prefix)) {
    						path = path.substr(prefix.length) || '/';
    					} else {
    						return null;
    					}
    				} else if (prefix instanceof RegExp) {
    					const match = path.match(prefix);

    					if (match && match[0]) {
    						path = path.substr(match[0].length) || '/';
    					} else {
    						return null;
    					}
    				}
    			}

    			// Check if the pattern matches
    			const matches = this._pattern.exec(path);

    			if (matches === null) {
    				return null;
    			}

    			// If the input was a regular expression, this._keys would be false, so return matches as is
    			if (this._keys === false) {
    				return matches;
    			}

    			const out = {};
    			let i = 0;

    			while (i < this._keys.length) {
    				// In the match parameters, URL-decode all values
    				try {
    					out[this._keys[i]] = decodeURIComponent(matches[i + 1] || '') || null;
    				} catch(e) {
    					out[this._keys[i]] = null;
    				}

    				i++;
    			}

    			return out;
    		}

    		/**
     * Dictionary with route details passed to the pre-conditions functions, as well as the `routeLoading`, `routeLoaded` and `conditionsFailed` events
     * @typedef {Object} RouteDetail
     * @property {string|RegExp} route - Route matched as defined in the route definition (could be a string or a reguar expression object)
     * @property {string} location - Location path
     * @property {string} querystring - Querystring from the hash
     * @property {object} [userData] - Custom data passed by the user
     * @property {SvelteComponent} [component] - Svelte component (only in `routeLoaded` events)
     * @property {string} [name] - Name of the Svelte component (only in `routeLoaded` events)
     */
    		/**
     * Executes all conditions (if any) to control whether the route can be shown. Conditions are executed in the order they are defined, and if a condition fails, the following ones aren't executed.
     * 
     * @param {RouteDetail} detail - Route detail
     * @returns {boolean} Returns true if all the conditions succeeded
     */
    		async checkConditions(detail) {
    			for (let i = 0; i < this.conditions.length; i++) {
    				if (!await this.conditions[i](detail)) {
    					return false;
    				}
    			}

    			return true;
    		}
    	}

    	// Set up all routes
    	const routesList = [];

    	if (routes instanceof Map) {
    		// If it's a map, iterate on it right away
    		routes.forEach((route, path) => {
    			routesList.push(new RouteItem(path, route));
    		});
    	} else {
    		// We have an object, so iterate on its own properties
    		Object.keys(routes).forEach(path => {
    			routesList.push(new RouteItem(path, routes[path]));
    		});
    	}

    	// Props for the component to render
    	let component = null;

    	let componentParams = null;
    	let props = {};

    	// Event dispatcher from Svelte
    	const dispatch = createEventDispatcher();

    	// Just like dispatch, but executes on the next iteration of the event loop
    	async function dispatchNextTick(name, detail) {
    		// Execute this code when the current call stack is complete
    		await tick();

    		dispatch(name, detail);
    	}

    	// If this is set, then that means we have popped into this var the state of our last scroll position
    	let previousScrollState = null;

    	let popStateChanged = null;

    	if (restoreScrollState) {
    		popStateChanged = event => {
    			// If this event was from our history.replaceState, event.state will contain
    			// our scroll history. Otherwise, event.state will be null (like on forward
    			// navigation)
    			if (event.state && (event.state.__svelte_spa_router_scrollY || event.state.__svelte_spa_router_scrollX)) {
    				previousScrollState = event.state;
    			} else {
    				previousScrollState = null;
    			}
    		};

    		// This is removed in the destroy() invocation below
    		window.addEventListener('popstate', popStateChanged);

    		afterUpdate(() => {
    			restoreScroll(previousScrollState);
    		});
    	}

    	// Always have the latest value of loc
    	let lastLoc = null;

    	// Current object of the component loaded
    	let componentObj = null;

    	// Handle hash change events
    	// Listen to changes in the $loc store and update the page
    	// Do not use the $: syntax because it gets triggered by too many things
    	const unsubscribeLoc = loc.subscribe(async newLoc => {
    		lastLoc = newLoc;

    		// Find a route matching the location
    		let i = 0;

    		while (i < routesList.length) {
    			const match = routesList[i].match(newLoc.location);

    			if (!match) {
    				i++;
    				continue;
    			}

    			const detail = {
    				route: routesList[i].path,
    				location: newLoc.location,
    				querystring: newLoc.querystring,
    				userData: routesList[i].userData,
    				params: match && typeof match == 'object' && Object.keys(match).length
    				? match
    				: null
    			};

    			// Check if the route can be loaded - if all conditions succeed
    			if (!await routesList[i].checkConditions(detail)) {
    				// Don't display anything
    				$$invalidate(0, component = null);

    				componentObj = null;

    				// Trigger an event to notify the user, then exit
    				dispatchNextTick('conditionsFailed', detail);

    				return;
    			}

    			// Trigger an event to alert that we're loading the route
    			// We need to clone the object on every event invocation so we don't risk the object to be modified in the next tick
    			dispatchNextTick('routeLoading', Object.assign({}, detail));

    			// If there's a component to show while we're loading the route, display it
    			const obj = routesList[i].component;

    			// Do not replace the component if we're loading the same one as before, to avoid the route being unmounted and re-mounted
    			if (componentObj != obj) {
    				if (obj.loading) {
    					$$invalidate(0, component = obj.loading);
    					componentObj = obj;
    					$$invalidate(1, componentParams = obj.loadingParams);
    					$$invalidate(2, props = {});

    					// Trigger the routeLoaded event for the loading component
    					// Create a copy of detail so we don't modify the object for the dynamic route (and the dynamic route doesn't modify our object too)
    					dispatchNextTick('routeLoaded', Object.assign({}, detail, {
    						component,
    						name: component.name,
    						params: componentParams
    					}));
    				} else {
    					$$invalidate(0, component = null);
    					componentObj = null;
    				}

    				// Invoke the Promise
    				const loaded = await obj();

    				// Now that we're here, after the promise resolved, check if we still want this component, as the user might have navigated to another page in the meanwhile
    				if (newLoc != lastLoc) {
    					// Don't update the component, just exit
    					return;
    				}

    				// If there is a "default" property, which is used by async routes, then pick that
    				$$invalidate(0, component = loaded && loaded.default || loaded);

    				componentObj = obj;
    			}

    			// Set componentParams only if we have a match, to avoid a warning similar to `<Component> was created with unknown prop 'params'`
    			// Of course, this assumes that developers always add a "params" prop when they are expecting parameters
    			if (match && typeof match == 'object' && Object.keys(match).length) {
    				$$invalidate(1, componentParams = match);
    			} else {
    				$$invalidate(1, componentParams = null);
    			}

    			// Set static props, if any
    			$$invalidate(2, props = routesList[i].props);

    			// Dispatch the routeLoaded event then exit
    			// We need to clone the object on every event invocation so we don't risk the object to be modified in the next tick
    			dispatchNextTick('routeLoaded', Object.assign({}, detail, {
    				component,
    				name: component.name,
    				params: componentParams
    			})).then(() => {
    				params.set(componentParams);
    			});

    			return;
    		}

    		// If we're still here, there was no match, so show the empty component
    		$$invalidate(0, component = null);

    		componentObj = null;
    		params.set(undefined);
    	});

    	onDestroy(() => {
    		unsubscribeLoc();
    		popStateChanged && window.removeEventListener('popstate', popStateChanged);
    	});

    	const writable_props = ['routes', 'prefix', 'restoreScrollState'];

    	Object_1.keys($$props).forEach(key => {
    		if (!~writable_props.indexOf(key) && key.slice(0, 2) !== '$$' && key !== 'slot') console_1$2.warn(`<Router> was created with unknown prop '${key}'`);
    	});

    	function routeEvent_handler(event) {
    		bubble.call(this, $$self, event);
    	}

    	function routeEvent_handler_1(event) {
    		bubble.call(this, $$self, event);
    	}

    	$$self.$$set = $$props => {
    		if ('routes' in $$props) $$invalidate(3, routes = $$props.routes);
    		if ('prefix' in $$props) $$invalidate(4, prefix = $$props.prefix);
    		if ('restoreScrollState' in $$props) $$invalidate(5, restoreScrollState = $$props.restoreScrollState);
    	};

    	$$self.$capture_state = () => ({
    		readable,
    		writable,
    		derived,
    		tick,
    		_wrap: wrap$1,
    		wrap,
    		getLocation,
    		loc,
    		location,
    		querystring,
    		params,
    		push,
    		pop,
    		replace,
    		link,
    		restoreScroll,
    		updateLink,
    		linkOpts,
    		scrollstateHistoryHandler,
    		onDestroy,
    		createEventDispatcher,
    		afterUpdate,
    		parse: parse$1,
    		routes,
    		prefix,
    		restoreScrollState,
    		RouteItem,
    		routesList,
    		component,
    		componentParams,
    		props,
    		dispatch,
    		dispatchNextTick,
    		previousScrollState,
    		popStateChanged,
    		lastLoc,
    		componentObj,
    		unsubscribeLoc
    	});

    	$$self.$inject_state = $$props => {
    		if ('routes' in $$props) $$invalidate(3, routes = $$props.routes);
    		if ('prefix' in $$props) $$invalidate(4, prefix = $$props.prefix);
    		if ('restoreScrollState' in $$props) $$invalidate(5, restoreScrollState = $$props.restoreScrollState);
    		if ('component' in $$props) $$invalidate(0, component = $$props.component);
    		if ('componentParams' in $$props) $$invalidate(1, componentParams = $$props.componentParams);
    		if ('props' in $$props) $$invalidate(2, props = $$props.props);
    		if ('previousScrollState' in $$props) previousScrollState = $$props.previousScrollState;
    		if ('popStateChanged' in $$props) popStateChanged = $$props.popStateChanged;
    		if ('lastLoc' in $$props) lastLoc = $$props.lastLoc;
    		if ('componentObj' in $$props) componentObj = $$props.componentObj;
    	};

    	if ($$props && "$$inject" in $$props) {
    		$$self.$inject_state($$props.$$inject);
    	}

    	$$self.$$.update = () => {
    		if ($$self.$$.dirty & /*restoreScrollState*/ 32) {
    			// Update history.scrollRestoration depending on restoreScrollState
    			history.scrollRestoration = restoreScrollState ? 'manual' : 'auto';
    		}
    	};

    	return [
    		component,
    		componentParams,
    		props,
    		routes,
    		prefix,
    		restoreScrollState,
    		routeEvent_handler,
    		routeEvent_handler_1
    	];
    }

    class Router extends SvelteComponentDev {
    	constructor(options) {
    		super(options);

    		init(this, options, instance$c, create_fragment$c, safe_not_equal, {
    			routes: 3,
    			prefix: 4,
    			restoreScrollState: 5
    		});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Router",
    			options,
    			id: create_fragment$c.name
    		});
    	}

    	get routes() {
    		throw new Error_1("<Router>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set routes(value) {
    		throw new Error_1("<Router>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get prefix() {
    		throw new Error_1("<Router>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set prefix(value) {
    		throw new Error_1("<Router>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get restoreScrollState() {
    		throw new Error_1("<Router>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set restoreScrollState(value) {
    		throw new Error_1("<Router>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}
    }

    var isMergeableObject = function isMergeableObject(value) {
    	return isNonNullObject(value)
    		&& !isSpecial(value)
    };

    function isNonNullObject(value) {
    	return !!value && typeof value === 'object'
    }

    function isSpecial(value) {
    	var stringValue = Object.prototype.toString.call(value);

    	return stringValue === '[object RegExp]'
    		|| stringValue === '[object Date]'
    		|| isReactElement(value)
    }

    // see https://github.com/facebook/react/blob/b5ac963fb791d1298e7f396236383bc955f916c1/src/isomorphic/classic/element/ReactElement.js#L21-L25
    var canUseSymbol = typeof Symbol === 'function' && Symbol.for;
    var REACT_ELEMENT_TYPE = canUseSymbol ? Symbol.for('react.element') : 0xeac7;

    function isReactElement(value) {
    	return value.$$typeof === REACT_ELEMENT_TYPE
    }

    function emptyTarget(val) {
    	return Array.isArray(val) ? [] : {}
    }

    function cloneUnlessOtherwiseSpecified(value, options) {
    	return (options.clone !== false && options.isMergeableObject(value))
    		? deepmerge(emptyTarget(value), value, options)
    		: value
    }

    function defaultArrayMerge(target, source, options) {
    	return target.concat(source).map(function(element) {
    		return cloneUnlessOtherwiseSpecified(element, options)
    	})
    }

    function getMergeFunction(key, options) {
    	if (!options.customMerge) {
    		return deepmerge
    	}
    	var customMerge = options.customMerge(key);
    	return typeof customMerge === 'function' ? customMerge : deepmerge
    }

    function getEnumerableOwnPropertySymbols(target) {
    	return Object.getOwnPropertySymbols
    		? Object.getOwnPropertySymbols(target).filter(function(symbol) {
    			return Object.propertyIsEnumerable.call(target, symbol)
    		})
    		: []
    }

    function getKeys(target) {
    	return Object.keys(target).concat(getEnumerableOwnPropertySymbols(target))
    }

    function propertyIsOnObject(object, property) {
    	try {
    		return property in object
    	} catch(_) {
    		return false
    	}
    }

    // Protects from prototype poisoning and unexpected merging up the prototype chain.
    function propertyIsUnsafe(target, key) {
    	return propertyIsOnObject(target, key) // Properties are safe to merge if they don't exist in the target yet,
    		&& !(Object.hasOwnProperty.call(target, key) // unsafe if they exist up the prototype chain,
    			&& Object.propertyIsEnumerable.call(target, key)) // and also unsafe if they're nonenumerable.
    }

    function mergeObject(target, source, options) {
    	var destination = {};
    	if (options.isMergeableObject(target)) {
    		getKeys(target).forEach(function(key) {
    			destination[key] = cloneUnlessOtherwiseSpecified(target[key], options);
    		});
    	}
    	getKeys(source).forEach(function(key) {
    		if (propertyIsUnsafe(target, key)) {
    			return
    		}

    		if (propertyIsOnObject(target, key) && options.isMergeableObject(source[key])) {
    			destination[key] = getMergeFunction(key, options)(target[key], source[key], options);
    		} else {
    			destination[key] = cloneUnlessOtherwiseSpecified(source[key], options);
    		}
    	});
    	return destination
    }

    function deepmerge(target, source, options) {
    	options = options || {};
    	options.arrayMerge = options.arrayMerge || defaultArrayMerge;
    	options.isMergeableObject = options.isMergeableObject || isMergeableObject;
    	// cloneUnlessOtherwiseSpecified is added to `options` so that custom arrayMerge()
    	// implementations can use it. The caller may not replace it.
    	options.cloneUnlessOtherwiseSpecified = cloneUnlessOtherwiseSpecified;

    	var sourceIsArray = Array.isArray(source);
    	var targetIsArray = Array.isArray(target);
    	var sourceAndTargetTypesMatch = sourceIsArray === targetIsArray;

    	if (!sourceAndTargetTypesMatch) {
    		return cloneUnlessOtherwiseSpecified(source, options)
    	} else if (sourceIsArray) {
    		return options.arrayMerge(target, source, options)
    	} else {
    		return mergeObject(target, source, options)
    	}
    }

    deepmerge.all = function deepmergeAll(array, options) {
    	if (!Array.isArray(array)) {
    		throw new Error('first argument should be an array')
    	}

    	return array.reduce(function(prev, next) {
    		return deepmerge(prev, next, options)
    	}, {})
    };

    var deepmerge_1 = deepmerge;

    var cjs = deepmerge_1;

    /******************************************************************************
    Copyright (c) Microsoft Corporation.

    Permission to use, copy, modify, and/or distribute this software for any
    purpose with or without fee is hereby granted.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
    REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
    AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
    INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
    LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
    OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
    PERFORMANCE OF THIS SOFTWARE.
    ***************************************************************************** */
    /* global Reflect, Promise, SuppressedError, Symbol */

    var extendStatics = function(d, b) {
      extendStatics = Object.setPrototypeOf ||
          ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
          function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
      return extendStatics(d, b);
    };

    function __extends(d, b) {
      if (typeof b !== "function" && b !== null)
          throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
      extendStatics(d, b);
      function __() { this.constructor = d; }
      d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    }

    var __assign = function() {
      __assign = Object.assign || function __assign(t) {
          for (var s, i = 1, n = arguments.length; i < n; i++) {
              s = arguments[i];
              for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p)) t[p] = s[p];
          }
          return t;
      };
      return __assign.apply(this, arguments);
    };

    function __spreadArray(to, from, pack) {
      if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
          if (ar || !(i in from)) {
              if (!ar) ar = Array.prototype.slice.call(from, 0, i);
              ar[i] = from[i];
          }
      }
      return to.concat(ar || Array.prototype.slice.call(from));
    }

    typeof SuppressedError === "function" ? SuppressedError : function (error, suppressed, message) {
      var e = new Error(message);
      return e.name = "SuppressedError", e.error = error, e.suppressed = suppressed, e;
    };

    var ErrorKind;
    (function (ErrorKind) {
        /** Argument is unclosed (e.g. `{0`) */
        ErrorKind[ErrorKind["EXPECT_ARGUMENT_CLOSING_BRACE"] = 1] = "EXPECT_ARGUMENT_CLOSING_BRACE";
        /** Argument is empty (e.g. `{}`). */
        ErrorKind[ErrorKind["EMPTY_ARGUMENT"] = 2] = "EMPTY_ARGUMENT";
        /** Argument is malformed (e.g. `{foo!}``) */
        ErrorKind[ErrorKind["MALFORMED_ARGUMENT"] = 3] = "MALFORMED_ARGUMENT";
        /** Expect an argument type (e.g. `{foo,}`) */
        ErrorKind[ErrorKind["EXPECT_ARGUMENT_TYPE"] = 4] = "EXPECT_ARGUMENT_TYPE";
        /** Unsupported argument type (e.g. `{foo,foo}`) */
        ErrorKind[ErrorKind["INVALID_ARGUMENT_TYPE"] = 5] = "INVALID_ARGUMENT_TYPE";
        /** Expect an argument style (e.g. `{foo, number, }`) */
        ErrorKind[ErrorKind["EXPECT_ARGUMENT_STYLE"] = 6] = "EXPECT_ARGUMENT_STYLE";
        /** The number skeleton is invalid. */
        ErrorKind[ErrorKind["INVALID_NUMBER_SKELETON"] = 7] = "INVALID_NUMBER_SKELETON";
        /** The date time skeleton is invalid. */
        ErrorKind[ErrorKind["INVALID_DATE_TIME_SKELETON"] = 8] = "INVALID_DATE_TIME_SKELETON";
        /** Exepct a number skeleton following the `::` (e.g. `{foo, number, ::}`) */
        ErrorKind[ErrorKind["EXPECT_NUMBER_SKELETON"] = 9] = "EXPECT_NUMBER_SKELETON";
        /** Exepct a date time skeleton following the `::` (e.g. `{foo, date, ::}`) */
        ErrorKind[ErrorKind["EXPECT_DATE_TIME_SKELETON"] = 10] = "EXPECT_DATE_TIME_SKELETON";
        /** Unmatched apostrophes in the argument style (e.g. `{foo, number, 'test`) */
        ErrorKind[ErrorKind["UNCLOSED_QUOTE_IN_ARGUMENT_STYLE"] = 11] = "UNCLOSED_QUOTE_IN_ARGUMENT_STYLE";
        /** Missing select argument options (e.g. `{foo, select}`) */
        ErrorKind[ErrorKind["EXPECT_SELECT_ARGUMENT_OPTIONS"] = 12] = "EXPECT_SELECT_ARGUMENT_OPTIONS";
        /** Expecting an offset value in `plural` or `selectordinal` argument (e.g `{foo, plural, offset}`) */
        ErrorKind[ErrorKind["EXPECT_PLURAL_ARGUMENT_OFFSET_VALUE"] = 13] = "EXPECT_PLURAL_ARGUMENT_OFFSET_VALUE";
        /** Offset value in `plural` or `selectordinal` is invalid (e.g. `{foo, plural, offset: x}`) */
        ErrorKind[ErrorKind["INVALID_PLURAL_ARGUMENT_OFFSET_VALUE"] = 14] = "INVALID_PLURAL_ARGUMENT_OFFSET_VALUE";
        /** Expecting a selector in `select` argument (e.g `{foo, select}`) */
        ErrorKind[ErrorKind["EXPECT_SELECT_ARGUMENT_SELECTOR"] = 15] = "EXPECT_SELECT_ARGUMENT_SELECTOR";
        /** Expecting a selector in `plural` or `selectordinal` argument (e.g `{foo, plural}`) */
        ErrorKind[ErrorKind["EXPECT_PLURAL_ARGUMENT_SELECTOR"] = 16] = "EXPECT_PLURAL_ARGUMENT_SELECTOR";
        /** Expecting a message fragment after the `select` selector (e.g. `{foo, select, apple}`) */
        ErrorKind[ErrorKind["EXPECT_SELECT_ARGUMENT_SELECTOR_FRAGMENT"] = 17] = "EXPECT_SELECT_ARGUMENT_SELECTOR_FRAGMENT";
        /**
         * Expecting a message fragment after the `plural` or `selectordinal` selector
         * (e.g. `{foo, plural, one}`)
         */
        ErrorKind[ErrorKind["EXPECT_PLURAL_ARGUMENT_SELECTOR_FRAGMENT"] = 18] = "EXPECT_PLURAL_ARGUMENT_SELECTOR_FRAGMENT";
        /** Selector in `plural` or `selectordinal` is malformed (e.g. `{foo, plural, =x {#}}`) */
        ErrorKind[ErrorKind["INVALID_PLURAL_ARGUMENT_SELECTOR"] = 19] = "INVALID_PLURAL_ARGUMENT_SELECTOR";
        /**
         * Duplicate selectors in `plural` or `selectordinal` argument.
         * (e.g. {foo, plural, one {#} one {#}})
         */
        ErrorKind[ErrorKind["DUPLICATE_PLURAL_ARGUMENT_SELECTOR"] = 20] = "DUPLICATE_PLURAL_ARGUMENT_SELECTOR";
        /** Duplicate selectors in `select` argument.
         * (e.g. {foo, select, apple {apple} apple {apple}})
         */
        ErrorKind[ErrorKind["DUPLICATE_SELECT_ARGUMENT_SELECTOR"] = 21] = "DUPLICATE_SELECT_ARGUMENT_SELECTOR";
        /** Plural or select argument option must have `other` clause. */
        ErrorKind[ErrorKind["MISSING_OTHER_CLAUSE"] = 22] = "MISSING_OTHER_CLAUSE";
        /** The tag is malformed. (e.g. `<bold!>foo</bold!>) */
        ErrorKind[ErrorKind["INVALID_TAG"] = 23] = "INVALID_TAG";
        /** The tag name is invalid. (e.g. `<123>foo</123>`) */
        ErrorKind[ErrorKind["INVALID_TAG_NAME"] = 25] = "INVALID_TAG_NAME";
        /** The closing tag does not match the opening tag. (e.g. `<bold>foo</italic>`) */
        ErrorKind[ErrorKind["UNMATCHED_CLOSING_TAG"] = 26] = "UNMATCHED_CLOSING_TAG";
        /** The opening tag has unmatched closing tag. (e.g. `<bold>foo`) */
        ErrorKind[ErrorKind["UNCLOSED_TAG"] = 27] = "UNCLOSED_TAG";
    })(ErrorKind || (ErrorKind = {}));

    var TYPE;
    (function (TYPE) {
        /**
         * Raw text
         */
        TYPE[TYPE["literal"] = 0] = "literal";
        /**
         * Variable w/o any format, e.g `var` in `this is a {var}`
         */
        TYPE[TYPE["argument"] = 1] = "argument";
        /**
         * Variable w/ number format
         */
        TYPE[TYPE["number"] = 2] = "number";
        /**
         * Variable w/ date format
         */
        TYPE[TYPE["date"] = 3] = "date";
        /**
         * Variable w/ time format
         */
        TYPE[TYPE["time"] = 4] = "time";
        /**
         * Variable w/ select format
         */
        TYPE[TYPE["select"] = 5] = "select";
        /**
         * Variable w/ plural format
         */
        TYPE[TYPE["plural"] = 6] = "plural";
        /**
         * Only possible within plural argument.
         * This is the `#` symbol that will be substituted with the count.
         */
        TYPE[TYPE["pound"] = 7] = "pound";
        /**
         * XML-like tag
         */
        TYPE[TYPE["tag"] = 8] = "tag";
    })(TYPE || (TYPE = {}));
    var SKELETON_TYPE;
    (function (SKELETON_TYPE) {
        SKELETON_TYPE[SKELETON_TYPE["number"] = 0] = "number";
        SKELETON_TYPE[SKELETON_TYPE["dateTime"] = 1] = "dateTime";
    })(SKELETON_TYPE || (SKELETON_TYPE = {}));
    /**
     * Type Guards
     */
    function isLiteralElement(el) {
        return el.type === TYPE.literal;
    }
    function isArgumentElement(el) {
        return el.type === TYPE.argument;
    }
    function isNumberElement(el) {
        return el.type === TYPE.number;
    }
    function isDateElement(el) {
        return el.type === TYPE.date;
    }
    function isTimeElement(el) {
        return el.type === TYPE.time;
    }
    function isSelectElement(el) {
        return el.type === TYPE.select;
    }
    function isPluralElement(el) {
        return el.type === TYPE.plural;
    }
    function isPoundElement(el) {
        return el.type === TYPE.pound;
    }
    function isTagElement(el) {
        return el.type === TYPE.tag;
    }
    function isNumberSkeleton(el) {
        return !!(el && typeof el === 'object' && el.type === SKELETON_TYPE.number);
    }
    function isDateTimeSkeleton(el) {
        return !!(el && typeof el === 'object' && el.type === SKELETON_TYPE.dateTime);
    }

    // @generated from regex-gen.ts
    var SPACE_SEPARATOR_REGEX = /[ \xA0\u1680\u2000-\u200A\u202F\u205F\u3000]/;

    /**
     * https://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table
     * Credit: https://github.com/caridy/intl-datetimeformat-pattern/blob/master/index.js
     * with some tweaks
     */
    var DATE_TIME_REGEX = /(?:[Eec]{1,6}|G{1,5}|[Qq]{1,5}|(?:[yYur]+|U{1,5})|[ML]{1,5}|d{1,2}|D{1,3}|F{1}|[abB]{1,5}|[hkHK]{1,2}|w{1,2}|W{1}|m{1,2}|s{1,2}|[zZOvVxX]{1,4})(?=([^']*'[^']*')*[^']*$)/g;
    /**
     * Parse Date time skeleton into Intl.DateTimeFormatOptions
     * Ref: https://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table
     * @public
     * @param skeleton skeleton string
     */
    function parseDateTimeSkeleton(skeleton) {
        var result = {};
        skeleton.replace(DATE_TIME_REGEX, function (match) {
            var len = match.length;
            switch (match[0]) {
                // Era
                case 'G':
                    result.era = len === 4 ? 'long' : len === 5 ? 'narrow' : 'short';
                    break;
                // Year
                case 'y':
                    result.year = len === 2 ? '2-digit' : 'numeric';
                    break;
                case 'Y':
                case 'u':
                case 'U':
                case 'r':
                    throw new RangeError('`Y/u/U/r` (year) patterns are not supported, use `y` instead');
                // Quarter
                case 'q':
                case 'Q':
                    throw new RangeError('`q/Q` (quarter) patterns are not supported');
                // Month
                case 'M':
                case 'L':
                    result.month = ['numeric', '2-digit', 'short', 'long', 'narrow'][len - 1];
                    break;
                // Week
                case 'w':
                case 'W':
                    throw new RangeError('`w/W` (week) patterns are not supported');
                case 'd':
                    result.day = ['numeric', '2-digit'][len - 1];
                    break;
                case 'D':
                case 'F':
                case 'g':
                    throw new RangeError('`D/F/g` (day) patterns are not supported, use `d` instead');
                // Weekday
                case 'E':
                    result.weekday = len === 4 ? 'short' : len === 5 ? 'narrow' : 'short';
                    break;
                case 'e':
                    if (len < 4) {
                        throw new RangeError('`e..eee` (weekday) patterns are not supported');
                    }
                    result.weekday = ['short', 'long', 'narrow', 'short'][len - 4];
                    break;
                case 'c':
                    if (len < 4) {
                        throw new RangeError('`c..ccc` (weekday) patterns are not supported');
                    }
                    result.weekday = ['short', 'long', 'narrow', 'short'][len - 4];
                    break;
                // Period
                case 'a': // AM, PM
                    result.hour12 = true;
                    break;
                case 'b': // am, pm, noon, midnight
                case 'B': // flexible day periods
                    throw new RangeError('`b/B` (period) patterns are not supported, use `a` instead');
                // Hour
                case 'h':
                    result.hourCycle = 'h12';
                    result.hour = ['numeric', '2-digit'][len - 1];
                    break;
                case 'H':
                    result.hourCycle = 'h23';
                    result.hour = ['numeric', '2-digit'][len - 1];
                    break;
                case 'K':
                    result.hourCycle = 'h11';
                    result.hour = ['numeric', '2-digit'][len - 1];
                    break;
                case 'k':
                    result.hourCycle = 'h24';
                    result.hour = ['numeric', '2-digit'][len - 1];
                    break;
                case 'j':
                case 'J':
                case 'C':
                    throw new RangeError('`j/J/C` (hour) patterns are not supported, use `h/H/K/k` instead');
                // Minute
                case 'm':
                    result.minute = ['numeric', '2-digit'][len - 1];
                    break;
                // Second
                case 's':
                    result.second = ['numeric', '2-digit'][len - 1];
                    break;
                case 'S':
                case 'A':
                    throw new RangeError('`S/A` (second) patterns are not supported, use `s` instead');
                // Zone
                case 'z': // 1..3, 4: specific non-location format
                    result.timeZoneName = len < 4 ? 'short' : 'long';
                    break;
                case 'Z': // 1..3, 4, 5: The ISO8601 varios formats
                case 'O': // 1, 4: miliseconds in day short, long
                case 'v': // 1, 4: generic non-location format
                case 'V': // 1, 2, 3, 4: time zone ID or city
                case 'X': // 1, 2, 3, 4: The ISO8601 varios formats
                case 'x': // 1, 2, 3, 4: The ISO8601 varios formats
                    throw new RangeError('`Z/O/v/V/X/x` (timeZone) patterns are not supported, use `z` instead');
            }
            return '';
        });
        return result;
    }

    // @generated from regex-gen.ts
    var WHITE_SPACE_REGEX = /[\t-\r \x85\u200E\u200F\u2028\u2029]/i;

    function parseNumberSkeletonFromString(skeleton) {
        if (skeleton.length === 0) {
            throw new Error('Number skeleton cannot be empty');
        }
        // Parse the skeleton
        var stringTokens = skeleton
            .split(WHITE_SPACE_REGEX)
            .filter(function (x) { return x.length > 0; });
        var tokens = [];
        for (var _i = 0, stringTokens_1 = stringTokens; _i < stringTokens_1.length; _i++) {
            var stringToken = stringTokens_1[_i];
            var stemAndOptions = stringToken.split('/');
            if (stemAndOptions.length === 0) {
                throw new Error('Invalid number skeleton');
            }
            var stem = stemAndOptions[0], options = stemAndOptions.slice(1);
            for (var _a = 0, options_1 = options; _a < options_1.length; _a++) {
                var option = options_1[_a];
                if (option.length === 0) {
                    throw new Error('Invalid number skeleton');
                }
            }
            tokens.push({ stem: stem, options: options });
        }
        return tokens;
    }
    function icuUnitToEcma(unit) {
        return unit.replace(/^(.*?)-/, '');
    }
    var FRACTION_PRECISION_REGEX = /^\.(?:(0+)(\*)?|(#+)|(0+)(#+))$/g;
    var SIGNIFICANT_PRECISION_REGEX = /^(@+)?(\+|#+)?[rs]?$/g;
    var INTEGER_WIDTH_REGEX = /(\*)(0+)|(#+)(0+)|(0+)/g;
    var CONCISE_INTEGER_WIDTH_REGEX = /^(0+)$/;
    function parseSignificantPrecision(str) {
        var result = {};
        if (str[str.length - 1] === 'r') {
            result.roundingPriority = 'morePrecision';
        }
        else if (str[str.length - 1] === 's') {
            result.roundingPriority = 'lessPrecision';
        }
        str.replace(SIGNIFICANT_PRECISION_REGEX, function (_, g1, g2) {
            // @@@ case
            if (typeof g2 !== 'string') {
                result.minimumSignificantDigits = g1.length;
                result.maximumSignificantDigits = g1.length;
            }
            // @@@+ case
            else if (g2 === '+') {
                result.minimumSignificantDigits = g1.length;
            }
            // .### case
            else if (g1[0] === '#') {
                result.maximumSignificantDigits = g1.length;
            }
            // .@@## or .@@@ case
            else {
                result.minimumSignificantDigits = g1.length;
                result.maximumSignificantDigits =
                    g1.length + (typeof g2 === 'string' ? g2.length : 0);
            }
            return '';
        });
        return result;
    }
    function parseSign(str) {
        switch (str) {
            case 'sign-auto':
                return {
                    signDisplay: 'auto',
                };
            case 'sign-accounting':
            case '()':
                return {
                    currencySign: 'accounting',
                };
            case 'sign-always':
            case '+!':
                return {
                    signDisplay: 'always',
                };
            case 'sign-accounting-always':
            case '()!':
                return {
                    signDisplay: 'always',
                    currencySign: 'accounting',
                };
            case 'sign-except-zero':
            case '+?':
                return {
                    signDisplay: 'exceptZero',
                };
            case 'sign-accounting-except-zero':
            case '()?':
                return {
                    signDisplay: 'exceptZero',
                    currencySign: 'accounting',
                };
            case 'sign-never':
            case '+_':
                return {
                    signDisplay: 'never',
                };
        }
    }
    function parseConciseScientificAndEngineeringStem(stem) {
        // Engineering
        var result;
        if (stem[0] === 'E' && stem[1] === 'E') {
            result = {
                notation: 'engineering',
            };
            stem = stem.slice(2);
        }
        else if (stem[0] === 'E') {
            result = {
                notation: 'scientific',
            };
            stem = stem.slice(1);
        }
        if (result) {
            var signDisplay = stem.slice(0, 2);
            if (signDisplay === '+!') {
                result.signDisplay = 'always';
                stem = stem.slice(2);
            }
            else if (signDisplay === '+?') {
                result.signDisplay = 'exceptZero';
                stem = stem.slice(2);
            }
            if (!CONCISE_INTEGER_WIDTH_REGEX.test(stem)) {
                throw new Error('Malformed concise eng/scientific notation');
            }
            result.minimumIntegerDigits = stem.length;
        }
        return result;
    }
    function parseNotationOptions(opt) {
        var result = {};
        var signOpts = parseSign(opt);
        if (signOpts) {
            return signOpts;
        }
        return result;
    }
    /**
     * https://github.com/unicode-org/icu/blob/master/docs/userguide/format_parse/numbers/skeletons.md#skeleton-stems-and-options
     */
    function parseNumberSkeleton(tokens) {
        var result = {};
        for (var _i = 0, tokens_1 = tokens; _i < tokens_1.length; _i++) {
            var token = tokens_1[_i];
            switch (token.stem) {
                case 'percent':
                case '%':
                    result.style = 'percent';
                    continue;
                case '%x100':
                    result.style = 'percent';
                    result.scale = 100;
                    continue;
                case 'currency':
                    result.style = 'currency';
                    result.currency = token.options[0];
                    continue;
                case 'group-off':
                case ',_':
                    result.useGrouping = false;
                    continue;
                case 'precision-integer':
                case '.':
                    result.maximumFractionDigits = 0;
                    continue;
                case 'measure-unit':
                case 'unit':
                    result.style = 'unit';
                    result.unit = icuUnitToEcma(token.options[0]);
                    continue;
                case 'compact-short':
                case 'K':
                    result.notation = 'compact';
                    result.compactDisplay = 'short';
                    continue;
                case 'compact-long':
                case 'KK':
                    result.notation = 'compact';
                    result.compactDisplay = 'long';
                    continue;
                case 'scientific':
                    result = __assign(__assign(__assign({}, result), { notation: 'scientific' }), token.options.reduce(function (all, opt) { return (__assign(__assign({}, all), parseNotationOptions(opt))); }, {}));
                    continue;
                case 'engineering':
                    result = __assign(__assign(__assign({}, result), { notation: 'engineering' }), token.options.reduce(function (all, opt) { return (__assign(__assign({}, all), parseNotationOptions(opt))); }, {}));
                    continue;
                case 'notation-simple':
                    result.notation = 'standard';
                    continue;
                // https://github.com/unicode-org/icu/blob/master/icu4c/source/i18n/unicode/unumberformatter.h
                case 'unit-width-narrow':
                    result.currencyDisplay = 'narrowSymbol';
                    result.unitDisplay = 'narrow';
                    continue;
                case 'unit-width-short':
                    result.currencyDisplay = 'code';
                    result.unitDisplay = 'short';
                    continue;
                case 'unit-width-full-name':
                    result.currencyDisplay = 'name';
                    result.unitDisplay = 'long';
                    continue;
                case 'unit-width-iso-code':
                    result.currencyDisplay = 'symbol';
                    continue;
                case 'scale':
                    result.scale = parseFloat(token.options[0]);
                    continue;
                // https://unicode-org.github.io/icu/userguide/format_parse/numbers/skeletons.html#integer-width
                case 'integer-width':
                    if (token.options.length > 1) {
                        throw new RangeError('integer-width stems only accept a single optional option');
                    }
                    token.options[0].replace(INTEGER_WIDTH_REGEX, function (_, g1, g2, g3, g4, g5) {
                        if (g1) {
                            result.minimumIntegerDigits = g2.length;
                        }
                        else if (g3 && g4) {
                            throw new Error('We currently do not support maximum integer digits');
                        }
                        else if (g5) {
                            throw new Error('We currently do not support exact integer digits');
                        }
                        return '';
                    });
                    continue;
            }
            // https://unicode-org.github.io/icu/userguide/format_parse/numbers/skeletons.html#integer-width
            if (CONCISE_INTEGER_WIDTH_REGEX.test(token.stem)) {
                result.minimumIntegerDigits = token.stem.length;
                continue;
            }
            if (FRACTION_PRECISION_REGEX.test(token.stem)) {
                // Precision
                // https://unicode-org.github.io/icu/userguide/format_parse/numbers/skeletons.html#fraction-precision
                // precision-integer case
                if (token.options.length > 1) {
                    throw new RangeError('Fraction-precision stems only accept a single optional option');
                }
                token.stem.replace(FRACTION_PRECISION_REGEX, function (_, g1, g2, g3, g4, g5) {
                    // .000* case (before ICU67 it was .000+)
                    if (g2 === '*') {
                        result.minimumFractionDigits = g1.length;
                    }
                    // .### case
                    else if (g3 && g3[0] === '#') {
                        result.maximumFractionDigits = g3.length;
                    }
                    // .00## case
                    else if (g4 && g5) {
                        result.minimumFractionDigits = g4.length;
                        result.maximumFractionDigits = g4.length + g5.length;
                    }
                    else {
                        result.minimumFractionDigits = g1.length;
                        result.maximumFractionDigits = g1.length;
                    }
                    return '';
                });
                var opt = token.options[0];
                // https://unicode-org.github.io/icu/userguide/format_parse/numbers/skeletons.html#trailing-zero-display
                if (opt === 'w') {
                    result = __assign(__assign({}, result), { trailingZeroDisplay: 'stripIfInteger' });
                }
                else if (opt) {
                    result = __assign(__assign({}, result), parseSignificantPrecision(opt));
                }
                continue;
            }
            // https://unicode-org.github.io/icu/userguide/format_parse/numbers/skeletons.html#significant-digits-precision
            if (SIGNIFICANT_PRECISION_REGEX.test(token.stem)) {
                result = __assign(__assign({}, result), parseSignificantPrecision(token.stem));
                continue;
            }
            var signOpts = parseSign(token.stem);
            if (signOpts) {
                result = __assign(__assign({}, result), signOpts);
            }
            var conciseScientificAndEngineeringOpts = parseConciseScientificAndEngineeringStem(token.stem);
            if (conciseScientificAndEngineeringOpts) {
                result = __assign(__assign({}, result), conciseScientificAndEngineeringOpts);
            }
        }
        return result;
    }

    // @generated from time-data-gen.ts
    // prettier-ignore  
    var timeData = {
        "AX": [
            "H"
        ],
        "BQ": [
            "H"
        ],
        "CP": [
            "H"
        ],
        "CZ": [
            "H"
        ],
        "DK": [
            "H"
        ],
        "FI": [
            "H"
        ],
        "ID": [
            "H"
        ],
        "IS": [
            "H"
        ],
        "ML": [
            "H"
        ],
        "NE": [
            "H"
        ],
        "RU": [
            "H"
        ],
        "SE": [
            "H"
        ],
        "SJ": [
            "H"
        ],
        "SK": [
            "H"
        ],
        "AS": [
            "h",
            "H"
        ],
        "BT": [
            "h",
            "H"
        ],
        "DJ": [
            "h",
            "H"
        ],
        "ER": [
            "h",
            "H"
        ],
        "GH": [
            "h",
            "H"
        ],
        "IN": [
            "h",
            "H"
        ],
        "LS": [
            "h",
            "H"
        ],
        "PG": [
            "h",
            "H"
        ],
        "PW": [
            "h",
            "H"
        ],
        "SO": [
            "h",
            "H"
        ],
        "TO": [
            "h",
            "H"
        ],
        "VU": [
            "h",
            "H"
        ],
        "WS": [
            "h",
            "H"
        ],
        "001": [
            "H",
            "h"
        ],
        "AL": [
            "h",
            "H",
            "hB"
        ],
        "TD": [
            "h",
            "H",
            "hB"
        ],
        "ca-ES": [
            "H",
            "h",
            "hB"
        ],
        "CF": [
            "H",
            "h",
            "hB"
        ],
        "CM": [
            "H",
            "h",
            "hB"
        ],
        "fr-CA": [
            "H",
            "h",
            "hB"
        ],
        "gl-ES": [
            "H",
            "h",
            "hB"
        ],
        "it-CH": [
            "H",
            "h",
            "hB"
        ],
        "it-IT": [
            "H",
            "h",
            "hB"
        ],
        "LU": [
            "H",
            "h",
            "hB"
        ],
        "NP": [
            "H",
            "h",
            "hB"
        ],
        "PF": [
            "H",
            "h",
            "hB"
        ],
        "SC": [
            "H",
            "h",
            "hB"
        ],
        "SM": [
            "H",
            "h",
            "hB"
        ],
        "SN": [
            "H",
            "h",
            "hB"
        ],
        "TF": [
            "H",
            "h",
            "hB"
        ],
        "VA": [
            "H",
            "h",
            "hB"
        ],
        "CY": [
            "h",
            "H",
            "hb",
            "hB"
        ],
        "GR": [
            "h",
            "H",
            "hb",
            "hB"
        ],
        "CO": [
            "h",
            "H",
            "hB",
            "hb"
        ],
        "DO": [
            "h",
            "H",
            "hB",
            "hb"
        ],
        "KP": [
            "h",
            "H",
            "hB",
            "hb"
        ],
        "KR": [
            "h",
            "H",
            "hB",
            "hb"
        ],
        "NA": [
            "h",
            "H",
            "hB",
            "hb"
        ],
        "PA": [
            "h",
            "H",
            "hB",
            "hb"
        ],
        "PR": [
            "h",
            "H",
            "hB",
            "hb"
        ],
        "VE": [
            "h",
            "H",
            "hB",
            "hb"
        ],
        "AC": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "AI": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "BW": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "BZ": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "CC": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "CK": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "CX": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "DG": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "FK": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "GB": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "GG": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "GI": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "IE": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "IM": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "IO": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "JE": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "LT": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "MK": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "MN": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "MS": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "NF": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "NG": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "NR": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "NU": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "PN": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "SH": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "SX": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "TA": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "ZA": [
            "H",
            "h",
            "hb",
            "hB"
        ],
        "af-ZA": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "AR": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "CL": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "CR": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "CU": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "EA": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "es-BO": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "es-BR": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "es-EC": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "es-ES": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "es-GQ": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "es-PE": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "GT": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "HN": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "IC": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "KG": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "KM": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "LK": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "MA": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "MX": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "NI": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "PY": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "SV": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "UY": [
            "H",
            "h",
            "hB",
            "hb"
        ],
        "JP": [
            "H",
            "h",
            "K"
        ],
        "AD": [
            "H",
            "hB"
        ],
        "AM": [
            "H",
            "hB"
        ],
        "AO": [
            "H",
            "hB"
        ],
        "AT": [
            "H",
            "hB"
        ],
        "AW": [
            "H",
            "hB"
        ],
        "BE": [
            "H",
            "hB"
        ],
        "BF": [
            "H",
            "hB"
        ],
        "BJ": [
            "H",
            "hB"
        ],
        "BL": [
            "H",
            "hB"
        ],
        "BR": [
            "H",
            "hB"
        ],
        "CG": [
            "H",
            "hB"
        ],
        "CI": [
            "H",
            "hB"
        ],
        "CV": [
            "H",
            "hB"
        ],
        "DE": [
            "H",
            "hB"
        ],
        "EE": [
            "H",
            "hB"
        ],
        "FR": [
            "H",
            "hB"
        ],
        "GA": [
            "H",
            "hB"
        ],
        "GF": [
            "H",
            "hB"
        ],
        "GN": [
            "H",
            "hB"
        ],
        "GP": [
            "H",
            "hB"
        ],
        "GW": [
            "H",
            "hB"
        ],
        "HR": [
            "H",
            "hB"
        ],
        "IL": [
            "H",
            "hB"
        ],
        "IT": [
            "H",
            "hB"
        ],
        "KZ": [
            "H",
            "hB"
        ],
        "MC": [
            "H",
            "hB"
        ],
        "MD": [
            "H",
            "hB"
        ],
        "MF": [
            "H",
            "hB"
        ],
        "MQ": [
            "H",
            "hB"
        ],
        "MZ": [
            "H",
            "hB"
        ],
        "NC": [
            "H",
            "hB"
        ],
        "NL": [
            "H",
            "hB"
        ],
        "PM": [
            "H",
            "hB"
        ],
        "PT": [
            "H",
            "hB"
        ],
        "RE": [
            "H",
            "hB"
        ],
        "RO": [
            "H",
            "hB"
        ],
        "SI": [
            "H",
            "hB"
        ],
        "SR": [
            "H",
            "hB"
        ],
        "ST": [
            "H",
            "hB"
        ],
        "TG": [
            "H",
            "hB"
        ],
        "TR": [
            "H",
            "hB"
        ],
        "WF": [
            "H",
            "hB"
        ],
        "YT": [
            "H",
            "hB"
        ],
        "BD": [
            "h",
            "hB",
            "H"
        ],
        "PK": [
            "h",
            "hB",
            "H"
        ],
        "AZ": [
            "H",
            "hB",
            "h"
        ],
        "BA": [
            "H",
            "hB",
            "h"
        ],
        "BG": [
            "H",
            "hB",
            "h"
        ],
        "CH": [
            "H",
            "hB",
            "h"
        ],
        "GE": [
            "H",
            "hB",
            "h"
        ],
        "LI": [
            "H",
            "hB",
            "h"
        ],
        "ME": [
            "H",
            "hB",
            "h"
        ],
        "RS": [
            "H",
            "hB",
            "h"
        ],
        "UA": [
            "H",
            "hB",
            "h"
        ],
        "UZ": [
            "H",
            "hB",
            "h"
        ],
        "XK": [
            "H",
            "hB",
            "h"
        ],
        "AG": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "AU": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "BB": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "BM": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "BS": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "CA": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "DM": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "en-001": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "FJ": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "FM": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "GD": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "GM": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "GU": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "GY": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "JM": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "KI": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "KN": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "KY": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "LC": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "LR": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "MH": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "MP": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "MW": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "NZ": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "SB": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "SG": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "SL": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "SS": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "SZ": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "TC": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "TT": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "UM": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "US": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "VC": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "VG": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "VI": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "ZM": [
            "h",
            "hb",
            "H",
            "hB"
        ],
        "BO": [
            "H",
            "hB",
            "h",
            "hb"
        ],
        "EC": [
            "H",
            "hB",
            "h",
            "hb"
        ],
        "ES": [
            "H",
            "hB",
            "h",
            "hb"
        ],
        "GQ": [
            "H",
            "hB",
            "h",
            "hb"
        ],
        "PE": [
            "H",
            "hB",
            "h",
            "hb"
        ],
        "AE": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "ar-001": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "BH": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "DZ": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "EG": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "EH": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "HK": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "IQ": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "JO": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "KW": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "LB": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "LY": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "MO": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "MR": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "OM": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "PH": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "PS": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "QA": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "SA": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "SD": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "SY": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "TN": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "YE": [
            "h",
            "hB",
            "hb",
            "H"
        ],
        "AF": [
            "H",
            "hb",
            "hB",
            "h"
        ],
        "LA": [
            "H",
            "hb",
            "hB",
            "h"
        ],
        "CN": [
            "H",
            "hB",
            "hb",
            "h"
        ],
        "LV": [
            "H",
            "hB",
            "hb",
            "h"
        ],
        "TL": [
            "H",
            "hB",
            "hb",
            "h"
        ],
        "zu-ZA": [
            "H",
            "hB",
            "hb",
            "h"
        ],
        "CD": [
            "hB",
            "H"
        ],
        "IR": [
            "hB",
            "H"
        ],
        "hi-IN": [
            "hB",
            "h",
            "H"
        ],
        "kn-IN": [
            "hB",
            "h",
            "H"
        ],
        "ml-IN": [
            "hB",
            "h",
            "H"
        ],
        "te-IN": [
            "hB",
            "h",
            "H"
        ],
        "KH": [
            "hB",
            "h",
            "H",
            "hb"
        ],
        "ta-IN": [
            "hB",
            "h",
            "hb",
            "H"
        ],
        "BN": [
            "hb",
            "hB",
            "h",
            "H"
        ],
        "MY": [
            "hb",
            "hB",
            "h",
            "H"
        ],
        "ET": [
            "hB",
            "hb",
            "h",
            "H"
        ],
        "gu-IN": [
            "hB",
            "hb",
            "h",
            "H"
        ],
        "mr-IN": [
            "hB",
            "hb",
            "h",
            "H"
        ],
        "pa-IN": [
            "hB",
            "hb",
            "h",
            "H"
        ],
        "TW": [
            "hB",
            "hb",
            "h",
            "H"
        ],
        "KE": [
            "hB",
            "hb",
            "H",
            "h"
        ],
        "MM": [
            "hB",
            "hb",
            "H",
            "h"
        ],
        "TZ": [
            "hB",
            "hb",
            "H",
            "h"
        ],
        "UG": [
            "hB",
            "hb",
            "H",
            "h"
        ]
    };

    /**
     * Returns the best matching date time pattern if a date time skeleton
     * pattern is provided with a locale. Follows the Unicode specification:
     * https://www.unicode.org/reports/tr35/tr35-dates.html#table-mapping-requested-time-skeletons-to-patterns
     * @param skeleton date time skeleton pattern that possibly includes j, J or C
     * @param locale
     */
    function getBestPattern(skeleton, locale) {
        var skeletonCopy = '';
        for (var patternPos = 0; patternPos < skeleton.length; patternPos++) {
            var patternChar = skeleton.charAt(patternPos);
            if (patternChar === 'j') {
                var extraLength = 0;
                while (patternPos + 1 < skeleton.length &&
                    skeleton.charAt(patternPos + 1) === patternChar) {
                    extraLength++;
                    patternPos++;
                }
                var hourLen = 1 + (extraLength & 1);
                var dayPeriodLen = extraLength < 2 ? 1 : 3 + (extraLength >> 1);
                var dayPeriodChar = 'a';
                var hourChar = getDefaultHourSymbolFromLocale(locale);
                if (hourChar == 'H' || hourChar == 'k') {
                    dayPeriodLen = 0;
                }
                while (dayPeriodLen-- > 0) {
                    skeletonCopy += dayPeriodChar;
                }
                while (hourLen-- > 0) {
                    skeletonCopy = hourChar + skeletonCopy;
                }
            }
            else if (patternChar === 'J') {
                skeletonCopy += 'H';
            }
            else {
                skeletonCopy += patternChar;
            }
        }
        return skeletonCopy;
    }
    /**
     * Maps the [hour cycle type](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/hourCycle)
     * of the given `locale` to the corresponding time pattern.
     * @param locale
     */
    function getDefaultHourSymbolFromLocale(locale) {
        var hourCycle = locale.hourCycle;
        if (hourCycle === undefined &&
            // @ts-ignore hourCycle(s) is not identified yet
            locale.hourCycles &&
            // @ts-ignore
            locale.hourCycles.length) {
            // @ts-ignore
            hourCycle = locale.hourCycles[0];
        }
        if (hourCycle) {
            switch (hourCycle) {
                case 'h24':
                    return 'k';
                case 'h23':
                    return 'H';
                case 'h12':
                    return 'h';
                case 'h11':
                    return 'K';
                default:
                    throw new Error('Invalid hourCycle');
            }
        }
        // TODO: Once hourCycle is fully supported remove the following with data generation
        var languageTag = locale.language;
        var regionTag;
        if (languageTag !== 'root') {
            regionTag = locale.maximize().region;
        }
        var hourCycles = timeData[regionTag || ''] ||
            timeData[languageTag || ''] ||
            timeData["".concat(languageTag, "-001")] ||
            timeData['001'];
        return hourCycles[0];
    }

    var _a;
    var SPACE_SEPARATOR_START_REGEX = new RegExp("^".concat(SPACE_SEPARATOR_REGEX.source, "*"));
    var SPACE_SEPARATOR_END_REGEX = new RegExp("".concat(SPACE_SEPARATOR_REGEX.source, "*$"));
    function createLocation(start, end) {
        return { start: start, end: end };
    }
    // #region Ponyfills
    // Consolidate these variables up top for easier toggling during debugging
    var hasNativeStartsWith = !!String.prototype.startsWith;
    var hasNativeFromCodePoint = !!String.fromCodePoint;
    var hasNativeFromEntries = !!Object.fromEntries;
    var hasNativeCodePointAt = !!String.prototype.codePointAt;
    var hasTrimStart = !!String.prototype.trimStart;
    var hasTrimEnd = !!String.prototype.trimEnd;
    var hasNativeIsSafeInteger = !!Number.isSafeInteger;
    var isSafeInteger = hasNativeIsSafeInteger
        ? Number.isSafeInteger
        : function (n) {
            return (typeof n === 'number' &&
                isFinite(n) &&
                Math.floor(n) === n &&
                Math.abs(n) <= 0x1fffffffffffff);
        };
    // IE11 does not support y and u.
    var REGEX_SUPPORTS_U_AND_Y = true;
    try {
        var re = RE('([^\\p{White_Space}\\p{Pattern_Syntax}]*)', 'yu');
        /**
         * legacy Edge or Xbox One browser
         * Unicode flag support: supported
         * Pattern_Syntax support: not supported
         * See https://github.com/formatjs/formatjs/issues/2822
         */
        REGEX_SUPPORTS_U_AND_Y = ((_a = re.exec('a')) === null || _a === void 0 ? void 0 : _a[0]) === 'a';
    }
    catch (_) {
        REGEX_SUPPORTS_U_AND_Y = false;
    }
    var startsWith = hasNativeStartsWith
        ? // Native
            function startsWith(s, search, position) {
                return s.startsWith(search, position);
            }
        : // For IE11
            function startsWith(s, search, position) {
                return s.slice(position, position + search.length) === search;
            };
    var fromCodePoint = hasNativeFromCodePoint
        ? String.fromCodePoint
        : // IE11
            function fromCodePoint() {
                var codePoints = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    codePoints[_i] = arguments[_i];
                }
                var elements = '';
                var length = codePoints.length;
                var i = 0;
                var code;
                while (length > i) {
                    code = codePoints[i++];
                    if (code > 0x10ffff)
                        throw RangeError(code + ' is not a valid code point');
                    elements +=
                        code < 0x10000
                            ? String.fromCharCode(code)
                            : String.fromCharCode(((code -= 0x10000) >> 10) + 0xd800, (code % 0x400) + 0xdc00);
                }
                return elements;
            };
    var fromEntries = 
    // native
    hasNativeFromEntries
        ? Object.fromEntries
        : // Ponyfill
            function fromEntries(entries) {
                var obj = {};
                for (var _i = 0, entries_1 = entries; _i < entries_1.length; _i++) {
                    var _a = entries_1[_i], k = _a[0], v = _a[1];
                    obj[k] = v;
                }
                return obj;
            };
    var codePointAt = hasNativeCodePointAt
        ? // Native
            function codePointAt(s, index) {
                return s.codePointAt(index);
            }
        : // IE 11
            function codePointAt(s, index) {
                var size = s.length;
                if (index < 0 || index >= size) {
                    return undefined;
                }
                var first = s.charCodeAt(index);
                var second;
                return first < 0xd800 ||
                    first > 0xdbff ||
                    index + 1 === size ||
                    (second = s.charCodeAt(index + 1)) < 0xdc00 ||
                    second > 0xdfff
                    ? first
                    : ((first - 0xd800) << 10) + (second - 0xdc00) + 0x10000;
            };
    var trimStart = hasTrimStart
        ? // Native
            function trimStart(s) {
                return s.trimStart();
            }
        : // Ponyfill
            function trimStart(s) {
                return s.replace(SPACE_SEPARATOR_START_REGEX, '');
            };
    var trimEnd = hasTrimEnd
        ? // Native
            function trimEnd(s) {
                return s.trimEnd();
            }
        : // Ponyfill
            function trimEnd(s) {
                return s.replace(SPACE_SEPARATOR_END_REGEX, '');
            };
    // Prevent minifier to translate new RegExp to literal form that might cause syntax error on IE11.
    function RE(s, flag) {
        return new RegExp(s, flag);
    }
    // #endregion
    var matchIdentifierAtIndex;
    if (REGEX_SUPPORTS_U_AND_Y) {
        // Native
        var IDENTIFIER_PREFIX_RE_1 = RE('([^\\p{White_Space}\\p{Pattern_Syntax}]*)', 'yu');
        matchIdentifierAtIndex = function matchIdentifierAtIndex(s, index) {
            var _a;
            IDENTIFIER_PREFIX_RE_1.lastIndex = index;
            var match = IDENTIFIER_PREFIX_RE_1.exec(s);
            return (_a = match[1]) !== null && _a !== void 0 ? _a : '';
        };
    }
    else {
        // IE11
        matchIdentifierAtIndex = function matchIdentifierAtIndex(s, index) {
            var match = [];
            while (true) {
                var c = codePointAt(s, index);
                if (c === undefined || _isWhiteSpace(c) || _isPatternSyntax(c)) {
                    break;
                }
                match.push(c);
                index += c >= 0x10000 ? 2 : 1;
            }
            return fromCodePoint.apply(void 0, match);
        };
    }
    var Parser = /** @class */ (function () {
        function Parser(message, options) {
            if (options === void 0) { options = {}; }
            this.message = message;
            this.position = { offset: 0, line: 1, column: 1 };
            this.ignoreTag = !!options.ignoreTag;
            this.locale = options.locale;
            this.requiresOtherClause = !!options.requiresOtherClause;
            this.shouldParseSkeletons = !!options.shouldParseSkeletons;
        }
        Parser.prototype.parse = function () {
            if (this.offset() !== 0) {
                throw Error('parser can only be used once');
            }
            return this.parseMessage(0, '', false);
        };
        Parser.prototype.parseMessage = function (nestingLevel, parentArgType, expectingCloseTag) {
            var elements = [];
            while (!this.isEOF()) {
                var char = this.char();
                if (char === 123 /* `{` */) {
                    var result = this.parseArgument(nestingLevel, expectingCloseTag);
                    if (result.err) {
                        return result;
                    }
                    elements.push(result.val);
                }
                else if (char === 125 /* `}` */ && nestingLevel > 0) {
                    break;
                }
                else if (char === 35 /* `#` */ &&
                    (parentArgType === 'plural' || parentArgType === 'selectordinal')) {
                    var position = this.clonePosition();
                    this.bump();
                    elements.push({
                        type: TYPE.pound,
                        location: createLocation(position, this.clonePosition()),
                    });
                }
                else if (char === 60 /* `<` */ &&
                    !this.ignoreTag &&
                    this.peek() === 47 // char code for '/'
                ) {
                    if (expectingCloseTag) {
                        break;
                    }
                    else {
                        return this.error(ErrorKind.UNMATCHED_CLOSING_TAG, createLocation(this.clonePosition(), this.clonePosition()));
                    }
                }
                else if (char === 60 /* `<` */ &&
                    !this.ignoreTag &&
                    _isAlpha(this.peek() || 0)) {
                    var result = this.parseTag(nestingLevel, parentArgType);
                    if (result.err) {
                        return result;
                    }
                    elements.push(result.val);
                }
                else {
                    var result = this.parseLiteral(nestingLevel, parentArgType);
                    if (result.err) {
                        return result;
                    }
                    elements.push(result.val);
                }
            }
            return { val: elements, err: null };
        };
        /**
         * A tag name must start with an ASCII lower/upper case letter. The grammar is based on the
         * [custom element name][] except that a dash is NOT always mandatory and uppercase letters
         * are accepted:
         *
         * ```
         * tag ::= "<" tagName (whitespace)* "/>" | "<" tagName (whitespace)* ">" message "</" tagName (whitespace)* ">"
         * tagName ::= [a-z] (PENChar)*
         * PENChar ::=
         *     "-" | "." | [0-9] | "_" | [a-z] | [A-Z] | #xB7 | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x37D] |
         *     [#x37F-#x1FFF] | [#x200C-#x200D] | [#x203F-#x2040] | [#x2070-#x218F] | [#x2C00-#x2FEF] |
         *     [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
         * ```
         *
         * [custom element name]: https://html.spec.whatwg.org/multipage/custom-elements.html#valid-custom-element-name
         * NOTE: We're a bit more lax here since HTML technically does not allow uppercase HTML element but we do
         * since other tag-based engines like React allow it
         */
        Parser.prototype.parseTag = function (nestingLevel, parentArgType) {
            var startPosition = this.clonePosition();
            this.bump(); // `<`
            var tagName = this.parseTagName();
            this.bumpSpace();
            if (this.bumpIf('/>')) {
                // Self closing tag
                return {
                    val: {
                        type: TYPE.literal,
                        value: "<".concat(tagName, "/>"),
                        location: createLocation(startPosition, this.clonePosition()),
                    },
                    err: null,
                };
            }
            else if (this.bumpIf('>')) {
                var childrenResult = this.parseMessage(nestingLevel + 1, parentArgType, true);
                if (childrenResult.err) {
                    return childrenResult;
                }
                var children = childrenResult.val;
                // Expecting a close tag
                var endTagStartPosition = this.clonePosition();
                if (this.bumpIf('</')) {
                    if (this.isEOF() || !_isAlpha(this.char())) {
                        return this.error(ErrorKind.INVALID_TAG, createLocation(endTagStartPosition, this.clonePosition()));
                    }
                    var closingTagNameStartPosition = this.clonePosition();
                    var closingTagName = this.parseTagName();
                    if (tagName !== closingTagName) {
                        return this.error(ErrorKind.UNMATCHED_CLOSING_TAG, createLocation(closingTagNameStartPosition, this.clonePosition()));
                    }
                    this.bumpSpace();
                    if (!this.bumpIf('>')) {
                        return this.error(ErrorKind.INVALID_TAG, createLocation(endTagStartPosition, this.clonePosition()));
                    }
                    return {
                        val: {
                            type: TYPE.tag,
                            value: tagName,
                            children: children,
                            location: createLocation(startPosition, this.clonePosition()),
                        },
                        err: null,
                    };
                }
                else {
                    return this.error(ErrorKind.UNCLOSED_TAG, createLocation(startPosition, this.clonePosition()));
                }
            }
            else {
                return this.error(ErrorKind.INVALID_TAG, createLocation(startPosition, this.clonePosition()));
            }
        };
        /**
         * This method assumes that the caller has peeked ahead for the first tag character.
         */
        Parser.prototype.parseTagName = function () {
            var startOffset = this.offset();
            this.bump(); // the first tag name character
            while (!this.isEOF() && _isPotentialElementNameChar(this.char())) {
                this.bump();
            }
            return this.message.slice(startOffset, this.offset());
        };
        Parser.prototype.parseLiteral = function (nestingLevel, parentArgType) {
            var start = this.clonePosition();
            var value = '';
            while (true) {
                var parseQuoteResult = this.tryParseQuote(parentArgType);
                if (parseQuoteResult) {
                    value += parseQuoteResult;
                    continue;
                }
                var parseUnquotedResult = this.tryParseUnquoted(nestingLevel, parentArgType);
                if (parseUnquotedResult) {
                    value += parseUnquotedResult;
                    continue;
                }
                var parseLeftAngleResult = this.tryParseLeftAngleBracket();
                if (parseLeftAngleResult) {
                    value += parseLeftAngleResult;
                    continue;
                }
                break;
            }
            var location = createLocation(start, this.clonePosition());
            return {
                val: { type: TYPE.literal, value: value, location: location },
                err: null,
            };
        };
        Parser.prototype.tryParseLeftAngleBracket = function () {
            if (!this.isEOF() &&
                this.char() === 60 /* `<` */ &&
                (this.ignoreTag ||
                    // If at the opening tag or closing tag position, bail.
                    !_isAlphaOrSlash(this.peek() || 0))) {
                this.bump(); // `<`
                return '<';
            }
            return null;
        };
        /**
         * Starting with ICU 4.8, an ASCII apostrophe only starts quoted text if it immediately precedes
         * a character that requires quoting (that is, "only where needed"), and works the same in
         * nested messages as on the top level of the pattern. The new behavior is otherwise compatible.
         */
        Parser.prototype.tryParseQuote = function (parentArgType) {
            if (this.isEOF() || this.char() !== 39 /* `'` */) {
                return null;
            }
            // Parse escaped char following the apostrophe, or early return if there is no escaped char.
            // Check if is valid escaped character
            switch (this.peek()) {
                case 39 /* `'` */:
                    // double quote, should return as a single quote.
                    this.bump();
                    this.bump();
                    return "'";
                // '{', '<', '>', '}'
                case 123:
                case 60:
                case 62:
                case 125:
                    break;
                case 35: // '#'
                    if (parentArgType === 'plural' || parentArgType === 'selectordinal') {
                        break;
                    }
                    return null;
                default:
                    return null;
            }
            this.bump(); // apostrophe
            var codePoints = [this.char()]; // escaped char
            this.bump();
            // read chars until the optional closing apostrophe is found
            while (!this.isEOF()) {
                var ch = this.char();
                if (ch === 39 /* `'` */) {
                    if (this.peek() === 39 /* `'` */) {
                        codePoints.push(39);
                        // Bump one more time because we need to skip 2 characters.
                        this.bump();
                    }
                    else {
                        // Optional closing apostrophe.
                        this.bump();
                        break;
                    }
                }
                else {
                    codePoints.push(ch);
                }
                this.bump();
            }
            return fromCodePoint.apply(void 0, codePoints);
        };
        Parser.prototype.tryParseUnquoted = function (nestingLevel, parentArgType) {
            if (this.isEOF()) {
                return null;
            }
            var ch = this.char();
            if (ch === 60 /* `<` */ ||
                ch === 123 /* `{` */ ||
                (ch === 35 /* `#` */ &&
                    (parentArgType === 'plural' || parentArgType === 'selectordinal')) ||
                (ch === 125 /* `}` */ && nestingLevel > 0)) {
                return null;
            }
            else {
                this.bump();
                return fromCodePoint(ch);
            }
        };
        Parser.prototype.parseArgument = function (nestingLevel, expectingCloseTag) {
            var openingBracePosition = this.clonePosition();
            this.bump(); // `{`
            this.bumpSpace();
            if (this.isEOF()) {
                return this.error(ErrorKind.EXPECT_ARGUMENT_CLOSING_BRACE, createLocation(openingBracePosition, this.clonePosition()));
            }
            if (this.char() === 125 /* `}` */) {
                this.bump();
                return this.error(ErrorKind.EMPTY_ARGUMENT, createLocation(openingBracePosition, this.clonePosition()));
            }
            // argument name
            var value = this.parseIdentifierIfPossible().value;
            if (!value) {
                return this.error(ErrorKind.MALFORMED_ARGUMENT, createLocation(openingBracePosition, this.clonePosition()));
            }
            this.bumpSpace();
            if (this.isEOF()) {
                return this.error(ErrorKind.EXPECT_ARGUMENT_CLOSING_BRACE, createLocation(openingBracePosition, this.clonePosition()));
            }
            switch (this.char()) {
                // Simple argument: `{name}`
                case 125 /* `}` */: {
                    this.bump(); // `}`
                    return {
                        val: {
                            type: TYPE.argument,
                            // value does not include the opening and closing braces.
                            value: value,
                            location: createLocation(openingBracePosition, this.clonePosition()),
                        },
                        err: null,
                    };
                }
                // Argument with options: `{name, format, ...}`
                case 44 /* `,` */: {
                    this.bump(); // `,`
                    this.bumpSpace();
                    if (this.isEOF()) {
                        return this.error(ErrorKind.EXPECT_ARGUMENT_CLOSING_BRACE, createLocation(openingBracePosition, this.clonePosition()));
                    }
                    return this.parseArgumentOptions(nestingLevel, expectingCloseTag, value, openingBracePosition);
                }
                default:
                    return this.error(ErrorKind.MALFORMED_ARGUMENT, createLocation(openingBracePosition, this.clonePosition()));
            }
        };
        /**
         * Advance the parser until the end of the identifier, if it is currently on
         * an identifier character. Return an empty string otherwise.
         */
        Parser.prototype.parseIdentifierIfPossible = function () {
            var startingPosition = this.clonePosition();
            var startOffset = this.offset();
            var value = matchIdentifierAtIndex(this.message, startOffset);
            var endOffset = startOffset + value.length;
            this.bumpTo(endOffset);
            var endPosition = this.clonePosition();
            var location = createLocation(startingPosition, endPosition);
            return { value: value, location: location };
        };
        Parser.prototype.parseArgumentOptions = function (nestingLevel, expectingCloseTag, value, openingBracePosition) {
            var _a;
            // Parse this range:
            // {name, type, style}
            //        ^---^
            var typeStartPosition = this.clonePosition();
            var argType = this.parseIdentifierIfPossible().value;
            var typeEndPosition = this.clonePosition();
            switch (argType) {
                case '':
                    // Expecting a style string number, date, time, plural, selectordinal, or select.
                    return this.error(ErrorKind.EXPECT_ARGUMENT_TYPE, createLocation(typeStartPosition, typeEndPosition));
                case 'number':
                case 'date':
                case 'time': {
                    // Parse this range:
                    // {name, number, style}
                    //              ^-------^
                    this.bumpSpace();
                    var styleAndLocation = null;
                    if (this.bumpIf(',')) {
                        this.bumpSpace();
                        var styleStartPosition = this.clonePosition();
                        var result = this.parseSimpleArgStyleIfPossible();
                        if (result.err) {
                            return result;
                        }
                        var style = trimEnd(result.val);
                        if (style.length === 0) {
                            return this.error(ErrorKind.EXPECT_ARGUMENT_STYLE, createLocation(this.clonePosition(), this.clonePosition()));
                        }
                        var styleLocation = createLocation(styleStartPosition, this.clonePosition());
                        styleAndLocation = { style: style, styleLocation: styleLocation };
                    }
                    var argCloseResult = this.tryParseArgumentClose(openingBracePosition);
                    if (argCloseResult.err) {
                        return argCloseResult;
                    }
                    var location_1 = createLocation(openingBracePosition, this.clonePosition());
                    // Extract style or skeleton
                    if (styleAndLocation && startsWith(styleAndLocation === null || styleAndLocation === void 0 ? void 0 : styleAndLocation.style, '::', 0)) {
                        // Skeleton starts with `::`.
                        var skeleton = trimStart(styleAndLocation.style.slice(2));
                        if (argType === 'number') {
                            var result = this.parseNumberSkeletonFromString(skeleton, styleAndLocation.styleLocation);
                            if (result.err) {
                                return result;
                            }
                            return {
                                val: { type: TYPE.number, value: value, location: location_1, style: result.val },
                                err: null,
                            };
                        }
                        else {
                            if (skeleton.length === 0) {
                                return this.error(ErrorKind.EXPECT_DATE_TIME_SKELETON, location_1);
                            }
                            var dateTimePattern = skeleton;
                            // Get "best match" pattern only if locale is passed, if not, let it
                            // pass as-is where `parseDateTimeSkeleton()` will throw an error
                            // for unsupported patterns.
                            if (this.locale) {
                                dateTimePattern = getBestPattern(skeleton, this.locale);
                            }
                            var style = {
                                type: SKELETON_TYPE.dateTime,
                                pattern: dateTimePattern,
                                location: styleAndLocation.styleLocation,
                                parsedOptions: this.shouldParseSkeletons
                                    ? parseDateTimeSkeleton(dateTimePattern)
                                    : {},
                            };
                            var type = argType === 'date' ? TYPE.date : TYPE.time;
                            return {
                                val: { type: type, value: value, location: location_1, style: style },
                                err: null,
                            };
                        }
                    }
                    // Regular style or no style.
                    return {
                        val: {
                            type: argType === 'number'
                                ? TYPE.number
                                : argType === 'date'
                                    ? TYPE.date
                                    : TYPE.time,
                            value: value,
                            location: location_1,
                            style: (_a = styleAndLocation === null || styleAndLocation === void 0 ? void 0 : styleAndLocation.style) !== null && _a !== void 0 ? _a : null,
                        },
                        err: null,
                    };
                }
                case 'plural':
                case 'selectordinal':
                case 'select': {
                    // Parse this range:
                    // {name, plural, options}
                    //              ^---------^
                    var typeEndPosition_1 = this.clonePosition();
                    this.bumpSpace();
                    if (!this.bumpIf(',')) {
                        return this.error(ErrorKind.EXPECT_SELECT_ARGUMENT_OPTIONS, createLocation(typeEndPosition_1, __assign({}, typeEndPosition_1)));
                    }
                    this.bumpSpace();
                    // Parse offset:
                    // {name, plural, offset:1, options}
                    //                ^-----^
                    //
                    // or the first option:
                    //
                    // {name, plural, one {...} other {...}}
                    //                ^--^
                    var identifierAndLocation = this.parseIdentifierIfPossible();
                    var pluralOffset = 0;
                    if (argType !== 'select' && identifierAndLocation.value === 'offset') {
                        if (!this.bumpIf(':')) {
                            return this.error(ErrorKind.EXPECT_PLURAL_ARGUMENT_OFFSET_VALUE, createLocation(this.clonePosition(), this.clonePosition()));
                        }
                        this.bumpSpace();
                        var result = this.tryParseDecimalInteger(ErrorKind.EXPECT_PLURAL_ARGUMENT_OFFSET_VALUE, ErrorKind.INVALID_PLURAL_ARGUMENT_OFFSET_VALUE);
                        if (result.err) {
                            return result;
                        }
                        // Parse another identifier for option parsing
                        this.bumpSpace();
                        identifierAndLocation = this.parseIdentifierIfPossible();
                        pluralOffset = result.val;
                    }
                    var optionsResult = this.tryParsePluralOrSelectOptions(nestingLevel, argType, expectingCloseTag, identifierAndLocation);
                    if (optionsResult.err) {
                        return optionsResult;
                    }
                    var argCloseResult = this.tryParseArgumentClose(openingBracePosition);
                    if (argCloseResult.err) {
                        return argCloseResult;
                    }
                    var location_2 = createLocation(openingBracePosition, this.clonePosition());
                    if (argType === 'select') {
                        return {
                            val: {
                                type: TYPE.select,
                                value: value,
                                options: fromEntries(optionsResult.val),
                                location: location_2,
                            },
                            err: null,
                        };
                    }
                    else {
                        return {
                            val: {
                                type: TYPE.plural,
                                value: value,
                                options: fromEntries(optionsResult.val),
                                offset: pluralOffset,
                                pluralType: argType === 'plural' ? 'cardinal' : 'ordinal',
                                location: location_2,
                            },
                            err: null,
                        };
                    }
                }
                default:
                    return this.error(ErrorKind.INVALID_ARGUMENT_TYPE, createLocation(typeStartPosition, typeEndPosition));
            }
        };
        Parser.prototype.tryParseArgumentClose = function (openingBracePosition) {
            // Parse: {value, number, ::currency/GBP }
            //
            if (this.isEOF() || this.char() !== 125 /* `}` */) {
                return this.error(ErrorKind.EXPECT_ARGUMENT_CLOSING_BRACE, createLocation(openingBracePosition, this.clonePosition()));
            }
            this.bump(); // `}`
            return { val: true, err: null };
        };
        /**
         * See: https://github.com/unicode-org/icu/blob/af7ed1f6d2298013dc303628438ec4abe1f16479/icu4c/source/common/messagepattern.cpp#L659
         */
        Parser.prototype.parseSimpleArgStyleIfPossible = function () {
            var nestedBraces = 0;
            var startPosition = this.clonePosition();
            while (!this.isEOF()) {
                var ch = this.char();
                switch (ch) {
                    case 39 /* `'` */: {
                        // Treat apostrophe as quoting but include it in the style part.
                        // Find the end of the quoted literal text.
                        this.bump();
                        var apostrophePosition = this.clonePosition();
                        if (!this.bumpUntil("'")) {
                            return this.error(ErrorKind.UNCLOSED_QUOTE_IN_ARGUMENT_STYLE, createLocation(apostrophePosition, this.clonePosition()));
                        }
                        this.bump();
                        break;
                    }
                    case 123 /* `{` */: {
                        nestedBraces += 1;
                        this.bump();
                        break;
                    }
                    case 125 /* `}` */: {
                        if (nestedBraces > 0) {
                            nestedBraces -= 1;
                        }
                        else {
                            return {
                                val: this.message.slice(startPosition.offset, this.offset()),
                                err: null,
                            };
                        }
                        break;
                    }
                    default:
                        this.bump();
                        break;
                }
            }
            return {
                val: this.message.slice(startPosition.offset, this.offset()),
                err: null,
            };
        };
        Parser.prototype.parseNumberSkeletonFromString = function (skeleton, location) {
            var tokens = [];
            try {
                tokens = parseNumberSkeletonFromString(skeleton);
            }
            catch (e) {
                return this.error(ErrorKind.INVALID_NUMBER_SKELETON, location);
            }
            return {
                val: {
                    type: SKELETON_TYPE.number,
                    tokens: tokens,
                    location: location,
                    parsedOptions: this.shouldParseSkeletons
                        ? parseNumberSkeleton(tokens)
                        : {},
                },
                err: null,
            };
        };
        /**
         * @param nesting_level The current nesting level of messages.
         *     This can be positive when parsing message fragment in select or plural argument options.
         * @param parent_arg_type The parent argument's type.
         * @param parsed_first_identifier If provided, this is the first identifier-like selector of
         *     the argument. It is a by-product of a previous parsing attempt.
         * @param expecting_close_tag If true, this message is directly or indirectly nested inside
         *     between a pair of opening and closing tags. The nested message will not parse beyond
         *     the closing tag boundary.
         */
        Parser.prototype.tryParsePluralOrSelectOptions = function (nestingLevel, parentArgType, expectCloseTag, parsedFirstIdentifier) {
            var _a;
            var hasOtherClause = false;
            var options = [];
            var parsedSelectors = new Set();
            var selector = parsedFirstIdentifier.value, selectorLocation = parsedFirstIdentifier.location;
            // Parse:
            // one {one apple}
            // ^--^
            while (true) {
                if (selector.length === 0) {
                    var startPosition = this.clonePosition();
                    if (parentArgType !== 'select' && this.bumpIf('=')) {
                        // Try parse `={number}` selector
                        var result = this.tryParseDecimalInteger(ErrorKind.EXPECT_PLURAL_ARGUMENT_SELECTOR, ErrorKind.INVALID_PLURAL_ARGUMENT_SELECTOR);
                        if (result.err) {
                            return result;
                        }
                        selectorLocation = createLocation(startPosition, this.clonePosition());
                        selector = this.message.slice(startPosition.offset, this.offset());
                    }
                    else {
                        break;
                    }
                }
                // Duplicate selector clauses
                if (parsedSelectors.has(selector)) {
                    return this.error(parentArgType === 'select'
                        ? ErrorKind.DUPLICATE_SELECT_ARGUMENT_SELECTOR
                        : ErrorKind.DUPLICATE_PLURAL_ARGUMENT_SELECTOR, selectorLocation);
                }
                if (selector === 'other') {
                    hasOtherClause = true;
                }
                // Parse:
                // one {one apple}
                //     ^----------^
                this.bumpSpace();
                var openingBracePosition = this.clonePosition();
                if (!this.bumpIf('{')) {
                    return this.error(parentArgType === 'select'
                        ? ErrorKind.EXPECT_SELECT_ARGUMENT_SELECTOR_FRAGMENT
                        : ErrorKind.EXPECT_PLURAL_ARGUMENT_SELECTOR_FRAGMENT, createLocation(this.clonePosition(), this.clonePosition()));
                }
                var fragmentResult = this.parseMessage(nestingLevel + 1, parentArgType, expectCloseTag);
                if (fragmentResult.err) {
                    return fragmentResult;
                }
                var argCloseResult = this.tryParseArgumentClose(openingBracePosition);
                if (argCloseResult.err) {
                    return argCloseResult;
                }
                options.push([
                    selector,
                    {
                        value: fragmentResult.val,
                        location: createLocation(openingBracePosition, this.clonePosition()),
                    },
                ]);
                // Keep track of the existing selectors
                parsedSelectors.add(selector);
                // Prep next selector clause.
                this.bumpSpace();
                (_a = this.parseIdentifierIfPossible(), selector = _a.value, selectorLocation = _a.location);
            }
            if (options.length === 0) {
                return this.error(parentArgType === 'select'
                    ? ErrorKind.EXPECT_SELECT_ARGUMENT_SELECTOR
                    : ErrorKind.EXPECT_PLURAL_ARGUMENT_SELECTOR, createLocation(this.clonePosition(), this.clonePosition()));
            }
            if (this.requiresOtherClause && !hasOtherClause) {
                return this.error(ErrorKind.MISSING_OTHER_CLAUSE, createLocation(this.clonePosition(), this.clonePosition()));
            }
            return { val: options, err: null };
        };
        Parser.prototype.tryParseDecimalInteger = function (expectNumberError, invalidNumberError) {
            var sign = 1;
            var startingPosition = this.clonePosition();
            if (this.bumpIf('+')) ;
            else if (this.bumpIf('-')) {
                sign = -1;
            }
            var hasDigits = false;
            var decimal = 0;
            while (!this.isEOF()) {
                var ch = this.char();
                if (ch >= 48 /* `0` */ && ch <= 57 /* `9` */) {
                    hasDigits = true;
                    decimal = decimal * 10 + (ch - 48);
                    this.bump();
                }
                else {
                    break;
                }
            }
            var location = createLocation(startingPosition, this.clonePosition());
            if (!hasDigits) {
                return this.error(expectNumberError, location);
            }
            decimal *= sign;
            if (!isSafeInteger(decimal)) {
                return this.error(invalidNumberError, location);
            }
            return { val: decimal, err: null };
        };
        Parser.prototype.offset = function () {
            return this.position.offset;
        };
        Parser.prototype.isEOF = function () {
            return this.offset() === this.message.length;
        };
        Parser.prototype.clonePosition = function () {
            // This is much faster than `Object.assign` or spread.
            return {
                offset: this.position.offset,
                line: this.position.line,
                column: this.position.column,
            };
        };
        /**
         * Return the code point at the current position of the parser.
         * Throws if the index is out of bound.
         */
        Parser.prototype.char = function () {
            var offset = this.position.offset;
            if (offset >= this.message.length) {
                throw Error('out of bound');
            }
            var code = codePointAt(this.message, offset);
            if (code === undefined) {
                throw Error("Offset ".concat(offset, " is at invalid UTF-16 code unit boundary"));
            }
            return code;
        };
        Parser.prototype.error = function (kind, location) {
            return {
                val: null,
                err: {
                    kind: kind,
                    message: this.message,
                    location: location,
                },
            };
        };
        /** Bump the parser to the next UTF-16 code unit. */
        Parser.prototype.bump = function () {
            if (this.isEOF()) {
                return;
            }
            var code = this.char();
            if (code === 10 /* '\n' */) {
                this.position.line += 1;
                this.position.column = 1;
                this.position.offset += 1;
            }
            else {
                this.position.column += 1;
                // 0 ~ 0x10000 -> unicode BMP, otherwise skip the surrogate pair.
                this.position.offset += code < 0x10000 ? 1 : 2;
            }
        };
        /**
         * If the substring starting at the current position of the parser has
         * the given prefix, then bump the parser to the character immediately
         * following the prefix and return true. Otherwise, don't bump the parser
         * and return false.
         */
        Parser.prototype.bumpIf = function (prefix) {
            if (startsWith(this.message, prefix, this.offset())) {
                for (var i = 0; i < prefix.length; i++) {
                    this.bump();
                }
                return true;
            }
            return false;
        };
        /**
         * Bump the parser until the pattern character is found and return `true`.
         * Otherwise bump to the end of the file and return `false`.
         */
        Parser.prototype.bumpUntil = function (pattern) {
            var currentOffset = this.offset();
            var index = this.message.indexOf(pattern, currentOffset);
            if (index >= 0) {
                this.bumpTo(index);
                return true;
            }
            else {
                this.bumpTo(this.message.length);
                return false;
            }
        };
        /**
         * Bump the parser to the target offset.
         * If target offset is beyond the end of the input, bump the parser to the end of the input.
         */
        Parser.prototype.bumpTo = function (targetOffset) {
            if (this.offset() > targetOffset) {
                throw Error("targetOffset ".concat(targetOffset, " must be greater than or equal to the current offset ").concat(this.offset()));
            }
            targetOffset = Math.min(targetOffset, this.message.length);
            while (true) {
                var offset = this.offset();
                if (offset === targetOffset) {
                    break;
                }
                if (offset > targetOffset) {
                    throw Error("targetOffset ".concat(targetOffset, " is at invalid UTF-16 code unit boundary"));
                }
                this.bump();
                if (this.isEOF()) {
                    break;
                }
            }
        };
        /** advance the parser through all whitespace to the next non-whitespace code unit. */
        Parser.prototype.bumpSpace = function () {
            while (!this.isEOF() && _isWhiteSpace(this.char())) {
                this.bump();
            }
        };
        /**
         * Peek at the *next* Unicode codepoint in the input without advancing the parser.
         * If the input has been exhausted, then this returns null.
         */
        Parser.prototype.peek = function () {
            if (this.isEOF()) {
                return null;
            }
            var code = this.char();
            var offset = this.offset();
            var nextCode = this.message.charCodeAt(offset + (code >= 0x10000 ? 2 : 1));
            return nextCode !== null && nextCode !== void 0 ? nextCode : null;
        };
        return Parser;
    }());
    /**
     * This check if codepoint is alphabet (lower & uppercase)
     * @param codepoint
     * @returns
     */
    function _isAlpha(codepoint) {
        return ((codepoint >= 97 && codepoint <= 122) ||
            (codepoint >= 65 && codepoint <= 90));
    }
    function _isAlphaOrSlash(codepoint) {
        return _isAlpha(codepoint) || codepoint === 47; /* '/' */
    }
    /** See `parseTag` function docs. */
    function _isPotentialElementNameChar(c) {
        return (c === 45 /* '-' */ ||
            c === 46 /* '.' */ ||
            (c >= 48 && c <= 57) /* 0..9 */ ||
            c === 95 /* '_' */ ||
            (c >= 97 && c <= 122) /** a..z */ ||
            (c >= 65 && c <= 90) /* A..Z */ ||
            c == 0xb7 ||
            (c >= 0xc0 && c <= 0xd6) ||
            (c >= 0xd8 && c <= 0xf6) ||
            (c >= 0xf8 && c <= 0x37d) ||
            (c >= 0x37f && c <= 0x1fff) ||
            (c >= 0x200c && c <= 0x200d) ||
            (c >= 0x203f && c <= 0x2040) ||
            (c >= 0x2070 && c <= 0x218f) ||
            (c >= 0x2c00 && c <= 0x2fef) ||
            (c >= 0x3001 && c <= 0xd7ff) ||
            (c >= 0xf900 && c <= 0xfdcf) ||
            (c >= 0xfdf0 && c <= 0xfffd) ||
            (c >= 0x10000 && c <= 0xeffff));
    }
    /**
     * Code point equivalent of regex `\p{White_Space}`.
     * From: https://www.unicode.org/Public/UCD/latest/ucd/PropList.txt
     */
    function _isWhiteSpace(c) {
        return ((c >= 0x0009 && c <= 0x000d) ||
            c === 0x0020 ||
            c === 0x0085 ||
            (c >= 0x200e && c <= 0x200f) ||
            c === 0x2028 ||
            c === 0x2029);
    }
    /**
     * Code point equivalent of regex `\p{Pattern_Syntax}`.
     * See https://www.unicode.org/Public/UCD/latest/ucd/PropList.txt
     */
    function _isPatternSyntax(c) {
        return ((c >= 0x0021 && c <= 0x0023) ||
            c === 0x0024 ||
            (c >= 0x0025 && c <= 0x0027) ||
            c === 0x0028 ||
            c === 0x0029 ||
            c === 0x002a ||
            c === 0x002b ||
            c === 0x002c ||
            c === 0x002d ||
            (c >= 0x002e && c <= 0x002f) ||
            (c >= 0x003a && c <= 0x003b) ||
            (c >= 0x003c && c <= 0x003e) ||
            (c >= 0x003f && c <= 0x0040) ||
            c === 0x005b ||
            c === 0x005c ||
            c === 0x005d ||
            c === 0x005e ||
            c === 0x0060 ||
            c === 0x007b ||
            c === 0x007c ||
            c === 0x007d ||
            c === 0x007e ||
            c === 0x00a1 ||
            (c >= 0x00a2 && c <= 0x00a5) ||
            c === 0x00a6 ||
            c === 0x00a7 ||
            c === 0x00a9 ||
            c === 0x00ab ||
            c === 0x00ac ||
            c === 0x00ae ||
            c === 0x00b0 ||
            c === 0x00b1 ||
            c === 0x00b6 ||
            c === 0x00bb ||
            c === 0x00bf ||
            c === 0x00d7 ||
            c === 0x00f7 ||
            (c >= 0x2010 && c <= 0x2015) ||
            (c >= 0x2016 && c <= 0x2017) ||
            c === 0x2018 ||
            c === 0x2019 ||
            c === 0x201a ||
            (c >= 0x201b && c <= 0x201c) ||
            c === 0x201d ||
            c === 0x201e ||
            c === 0x201f ||
            (c >= 0x2020 && c <= 0x2027) ||
            (c >= 0x2030 && c <= 0x2038) ||
            c === 0x2039 ||
            c === 0x203a ||
            (c >= 0x203b && c <= 0x203e) ||
            (c >= 0x2041 && c <= 0x2043) ||
            c === 0x2044 ||
            c === 0x2045 ||
            c === 0x2046 ||
            (c >= 0x2047 && c <= 0x2051) ||
            c === 0x2052 ||
            c === 0x2053 ||
            (c >= 0x2055 && c <= 0x205e) ||
            (c >= 0x2190 && c <= 0x2194) ||
            (c >= 0x2195 && c <= 0x2199) ||
            (c >= 0x219a && c <= 0x219b) ||
            (c >= 0x219c && c <= 0x219f) ||
            c === 0x21a0 ||
            (c >= 0x21a1 && c <= 0x21a2) ||
            c === 0x21a3 ||
            (c >= 0x21a4 && c <= 0x21a5) ||
            c === 0x21a6 ||
            (c >= 0x21a7 && c <= 0x21ad) ||
            c === 0x21ae ||
            (c >= 0x21af && c <= 0x21cd) ||
            (c >= 0x21ce && c <= 0x21cf) ||
            (c >= 0x21d0 && c <= 0x21d1) ||
            c === 0x21d2 ||
            c === 0x21d3 ||
            c === 0x21d4 ||
            (c >= 0x21d5 && c <= 0x21f3) ||
            (c >= 0x21f4 && c <= 0x22ff) ||
            (c >= 0x2300 && c <= 0x2307) ||
            c === 0x2308 ||
            c === 0x2309 ||
            c === 0x230a ||
            c === 0x230b ||
            (c >= 0x230c && c <= 0x231f) ||
            (c >= 0x2320 && c <= 0x2321) ||
            (c >= 0x2322 && c <= 0x2328) ||
            c === 0x2329 ||
            c === 0x232a ||
            (c >= 0x232b && c <= 0x237b) ||
            c === 0x237c ||
            (c >= 0x237d && c <= 0x239a) ||
            (c >= 0x239b && c <= 0x23b3) ||
            (c >= 0x23b4 && c <= 0x23db) ||
            (c >= 0x23dc && c <= 0x23e1) ||
            (c >= 0x23e2 && c <= 0x2426) ||
            (c >= 0x2427 && c <= 0x243f) ||
            (c >= 0x2440 && c <= 0x244a) ||
            (c >= 0x244b && c <= 0x245f) ||
            (c >= 0x2500 && c <= 0x25b6) ||
            c === 0x25b7 ||
            (c >= 0x25b8 && c <= 0x25c0) ||
            c === 0x25c1 ||
            (c >= 0x25c2 && c <= 0x25f7) ||
            (c >= 0x25f8 && c <= 0x25ff) ||
            (c >= 0x2600 && c <= 0x266e) ||
            c === 0x266f ||
            (c >= 0x2670 && c <= 0x2767) ||
            c === 0x2768 ||
            c === 0x2769 ||
            c === 0x276a ||
            c === 0x276b ||
            c === 0x276c ||
            c === 0x276d ||
            c === 0x276e ||
            c === 0x276f ||
            c === 0x2770 ||
            c === 0x2771 ||
            c === 0x2772 ||
            c === 0x2773 ||
            c === 0x2774 ||
            c === 0x2775 ||
            (c >= 0x2794 && c <= 0x27bf) ||
            (c >= 0x27c0 && c <= 0x27c4) ||
            c === 0x27c5 ||
            c === 0x27c6 ||
            (c >= 0x27c7 && c <= 0x27e5) ||
            c === 0x27e6 ||
            c === 0x27e7 ||
            c === 0x27e8 ||
            c === 0x27e9 ||
            c === 0x27ea ||
            c === 0x27eb ||
            c === 0x27ec ||
            c === 0x27ed ||
            c === 0x27ee ||
            c === 0x27ef ||
            (c >= 0x27f0 && c <= 0x27ff) ||
            (c >= 0x2800 && c <= 0x28ff) ||
            (c >= 0x2900 && c <= 0x2982) ||
            c === 0x2983 ||
            c === 0x2984 ||
            c === 0x2985 ||
            c === 0x2986 ||
            c === 0x2987 ||
            c === 0x2988 ||
            c === 0x2989 ||
            c === 0x298a ||
            c === 0x298b ||
            c === 0x298c ||
            c === 0x298d ||
            c === 0x298e ||
            c === 0x298f ||
            c === 0x2990 ||
            c === 0x2991 ||
            c === 0x2992 ||
            c === 0x2993 ||
            c === 0x2994 ||
            c === 0x2995 ||
            c === 0x2996 ||
            c === 0x2997 ||
            c === 0x2998 ||
            (c >= 0x2999 && c <= 0x29d7) ||
            c === 0x29d8 ||
            c === 0x29d9 ||
            c === 0x29da ||
            c === 0x29db ||
            (c >= 0x29dc && c <= 0x29fb) ||
            c === 0x29fc ||
            c === 0x29fd ||
            (c >= 0x29fe && c <= 0x2aff) ||
            (c >= 0x2b00 && c <= 0x2b2f) ||
            (c >= 0x2b30 && c <= 0x2b44) ||
            (c >= 0x2b45 && c <= 0x2b46) ||
            (c >= 0x2b47 && c <= 0x2b4c) ||
            (c >= 0x2b4d && c <= 0x2b73) ||
            (c >= 0x2b74 && c <= 0x2b75) ||
            (c >= 0x2b76 && c <= 0x2b95) ||
            c === 0x2b96 ||
            (c >= 0x2b97 && c <= 0x2bff) ||
            (c >= 0x2e00 && c <= 0x2e01) ||
            c === 0x2e02 ||
            c === 0x2e03 ||
            c === 0x2e04 ||
            c === 0x2e05 ||
            (c >= 0x2e06 && c <= 0x2e08) ||
            c === 0x2e09 ||
            c === 0x2e0a ||
            c === 0x2e0b ||
            c === 0x2e0c ||
            c === 0x2e0d ||
            (c >= 0x2e0e && c <= 0x2e16) ||
            c === 0x2e17 ||
            (c >= 0x2e18 && c <= 0x2e19) ||
            c === 0x2e1a ||
            c === 0x2e1b ||
            c === 0x2e1c ||
            c === 0x2e1d ||
            (c >= 0x2e1e && c <= 0x2e1f) ||
            c === 0x2e20 ||
            c === 0x2e21 ||
            c === 0x2e22 ||
            c === 0x2e23 ||
            c === 0x2e24 ||
            c === 0x2e25 ||
            c === 0x2e26 ||
            c === 0x2e27 ||
            c === 0x2e28 ||
            c === 0x2e29 ||
            (c >= 0x2e2a && c <= 0x2e2e) ||
            c === 0x2e2f ||
            (c >= 0x2e30 && c <= 0x2e39) ||
            (c >= 0x2e3a && c <= 0x2e3b) ||
            (c >= 0x2e3c && c <= 0x2e3f) ||
            c === 0x2e40 ||
            c === 0x2e41 ||
            c === 0x2e42 ||
            (c >= 0x2e43 && c <= 0x2e4f) ||
            (c >= 0x2e50 && c <= 0x2e51) ||
            c === 0x2e52 ||
            (c >= 0x2e53 && c <= 0x2e7f) ||
            (c >= 0x3001 && c <= 0x3003) ||
            c === 0x3008 ||
            c === 0x3009 ||
            c === 0x300a ||
            c === 0x300b ||
            c === 0x300c ||
            c === 0x300d ||
            c === 0x300e ||
            c === 0x300f ||
            c === 0x3010 ||
            c === 0x3011 ||
            (c >= 0x3012 && c <= 0x3013) ||
            c === 0x3014 ||
            c === 0x3015 ||
            c === 0x3016 ||
            c === 0x3017 ||
            c === 0x3018 ||
            c === 0x3019 ||
            c === 0x301a ||
            c === 0x301b ||
            c === 0x301c ||
            c === 0x301d ||
            (c >= 0x301e && c <= 0x301f) ||
            c === 0x3020 ||
            c === 0x3030 ||
            c === 0xfd3e ||
            c === 0xfd3f ||
            (c >= 0xfe45 && c <= 0xfe46));
    }

    function pruneLocation(els) {
        els.forEach(function (el) {
            delete el.location;
            if (isSelectElement(el) || isPluralElement(el)) {
                for (var k in el.options) {
                    delete el.options[k].location;
                    pruneLocation(el.options[k].value);
                }
            }
            else if (isNumberElement(el) && isNumberSkeleton(el.style)) {
                delete el.style.location;
            }
            else if ((isDateElement(el) || isTimeElement(el)) &&
                isDateTimeSkeleton(el.style)) {
                delete el.style.location;
            }
            else if (isTagElement(el)) {
                pruneLocation(el.children);
            }
        });
    }
    function parse(message, opts) {
        if (opts === void 0) { opts = {}; }
        opts = __assign({ shouldParseSkeletons: true, requiresOtherClause: true }, opts);
        var result = new Parser(message, opts).parse();
        if (result.err) {
            var error = SyntaxError(ErrorKind[result.err.kind]);
            // @ts-expect-error Assign to error object
            error.location = result.err.location;
            // @ts-expect-error Assign to error object
            error.originalMessage = result.err.message;
            throw error;
        }
        if (!(opts === null || opts === void 0 ? void 0 : opts.captureLocation)) {
            pruneLocation(result.val);
        }
        return result.val;
    }

    //
    // Main
    //
    function memoize(fn, options) {
        var cache = options && options.cache ? options.cache : cacheDefault;
        var serializer = options && options.serializer ? options.serializer : serializerDefault;
        var strategy = options && options.strategy ? options.strategy : strategyDefault;
        return strategy(fn, {
            cache: cache,
            serializer: serializer,
        });
    }
    //
    // Strategy
    //
    function isPrimitive(value) {
        return (value == null || typeof value === 'number' || typeof value === 'boolean'); // || typeof value === "string" 'unsafe' primitive for our needs
    }
    function monadic(fn, cache, serializer, arg) {
        var cacheKey = isPrimitive(arg) ? arg : serializer(arg);
        var computedValue = cache.get(cacheKey);
        if (typeof computedValue === 'undefined') {
            computedValue = fn.call(this, arg);
            cache.set(cacheKey, computedValue);
        }
        return computedValue;
    }
    function variadic(fn, cache, serializer) {
        var args = Array.prototype.slice.call(arguments, 3);
        var cacheKey = serializer(args);
        var computedValue = cache.get(cacheKey);
        if (typeof computedValue === 'undefined') {
            computedValue = fn.apply(this, args);
            cache.set(cacheKey, computedValue);
        }
        return computedValue;
    }
    function assemble(fn, context, strategy, cache, serialize) {
        return strategy.bind(context, fn, cache, serialize);
    }
    function strategyDefault(fn, options) {
        var strategy = fn.length === 1 ? monadic : variadic;
        return assemble(fn, this, strategy, options.cache.create(), options.serializer);
    }
    function strategyVariadic(fn, options) {
        return assemble(fn, this, variadic, options.cache.create(), options.serializer);
    }
    function strategyMonadic(fn, options) {
        return assemble(fn, this, monadic, options.cache.create(), options.serializer);
    }
    //
    // Serializer
    //
    var serializerDefault = function () {
        return JSON.stringify(arguments);
    };
    //
    // Cache
    //
    function ObjectWithoutPrototypeCache() {
        this.cache = Object.create(null);
    }
    ObjectWithoutPrototypeCache.prototype.get = function (key) {
        return this.cache[key];
    };
    ObjectWithoutPrototypeCache.prototype.set = function (key, value) {
        this.cache[key] = value;
    };
    var cacheDefault = {
        create: function create() {
            // @ts-ignore
            return new ObjectWithoutPrototypeCache();
        },
    };
    var strategies = {
        variadic: strategyVariadic,
        monadic: strategyMonadic,
    };

    var ErrorCode;
    (function (ErrorCode) {
        // When we have a placeholder but no value to format
        ErrorCode["MISSING_VALUE"] = "MISSING_VALUE";
        // When value supplied is invalid
        ErrorCode["INVALID_VALUE"] = "INVALID_VALUE";
        // When we need specific Intl API but it's not available
        ErrorCode["MISSING_INTL_API"] = "MISSING_INTL_API";
    })(ErrorCode || (ErrorCode = {}));
    var FormatError = /** @class */ (function (_super) {
        __extends(FormatError, _super);
        function FormatError(msg, code, originalMessage) {
            var _this = _super.call(this, msg) || this;
            _this.code = code;
            _this.originalMessage = originalMessage;
            return _this;
        }
        FormatError.prototype.toString = function () {
            return "[formatjs Error: ".concat(this.code, "] ").concat(this.message);
        };
        return FormatError;
    }(Error));
    var InvalidValueError = /** @class */ (function (_super) {
        __extends(InvalidValueError, _super);
        function InvalidValueError(variableId, value, options, originalMessage) {
            return _super.call(this, "Invalid values for \"".concat(variableId, "\": \"").concat(value, "\". Options are \"").concat(Object.keys(options).join('", "'), "\""), ErrorCode.INVALID_VALUE, originalMessage) || this;
        }
        return InvalidValueError;
    }(FormatError));
    var InvalidValueTypeError = /** @class */ (function (_super) {
        __extends(InvalidValueTypeError, _super);
        function InvalidValueTypeError(value, type, originalMessage) {
            return _super.call(this, "Value for \"".concat(value, "\" must be of type ").concat(type), ErrorCode.INVALID_VALUE, originalMessage) || this;
        }
        return InvalidValueTypeError;
    }(FormatError));
    var MissingValueError = /** @class */ (function (_super) {
        __extends(MissingValueError, _super);
        function MissingValueError(variableId, originalMessage) {
            return _super.call(this, "The intl string context variable \"".concat(variableId, "\" was not provided to the string \"").concat(originalMessage, "\""), ErrorCode.MISSING_VALUE, originalMessage) || this;
        }
        return MissingValueError;
    }(FormatError));

    var PART_TYPE;
    (function (PART_TYPE) {
        PART_TYPE[PART_TYPE["literal"] = 0] = "literal";
        PART_TYPE[PART_TYPE["object"] = 1] = "object";
    })(PART_TYPE || (PART_TYPE = {}));
    function mergeLiteral(parts) {
        if (parts.length < 2) {
            return parts;
        }
        return parts.reduce(function (all, part) {
            var lastPart = all[all.length - 1];
            if (!lastPart ||
                lastPart.type !== PART_TYPE.literal ||
                part.type !== PART_TYPE.literal) {
                all.push(part);
            }
            else {
                lastPart.value += part.value;
            }
            return all;
        }, []);
    }
    function isFormatXMLElementFn(el) {
        return typeof el === 'function';
    }
    // TODO(skeleton): add skeleton support
    function formatToParts(els, locales, formatters, formats, values, currentPluralValue, 
    // For debugging
    originalMessage) {
        // Hot path for straight simple msg translations
        if (els.length === 1 && isLiteralElement(els[0])) {
            return [
                {
                    type: PART_TYPE.literal,
                    value: els[0].value,
                },
            ];
        }
        var result = [];
        for (var _i = 0, els_1 = els; _i < els_1.length; _i++) {
            var el = els_1[_i];
            // Exit early for string parts.
            if (isLiteralElement(el)) {
                result.push({
                    type: PART_TYPE.literal,
                    value: el.value,
                });
                continue;
            }
            // TODO: should this part be literal type?
            // Replace `#` in plural rules with the actual numeric value.
            if (isPoundElement(el)) {
                if (typeof currentPluralValue === 'number') {
                    result.push({
                        type: PART_TYPE.literal,
                        value: formatters.getNumberFormat(locales).format(currentPluralValue),
                    });
                }
                continue;
            }
            var varName = el.value;
            // Enforce that all required values are provided by the caller.
            if (!(values && varName in values)) {
                throw new MissingValueError(varName, originalMessage);
            }
            var value = values[varName];
            if (isArgumentElement(el)) {
                if (!value || typeof value === 'string' || typeof value === 'number') {
                    value =
                        typeof value === 'string' || typeof value === 'number'
                            ? String(value)
                            : '';
                }
                result.push({
                    type: typeof value === 'string' ? PART_TYPE.literal : PART_TYPE.object,
                    value: value,
                });
                continue;
            }
            // Recursively format plural and select parts' option — which can be a
            // nested pattern structure. The choosing of the option to use is
            // abstracted-by and delegated-to the part helper object.
            if (isDateElement(el)) {
                var style = typeof el.style === 'string'
                    ? formats.date[el.style]
                    : isDateTimeSkeleton(el.style)
                        ? el.style.parsedOptions
                        : undefined;
                result.push({
                    type: PART_TYPE.literal,
                    value: formatters
                        .getDateTimeFormat(locales, style)
                        .format(value),
                });
                continue;
            }
            if (isTimeElement(el)) {
                var style = typeof el.style === 'string'
                    ? formats.time[el.style]
                    : isDateTimeSkeleton(el.style)
                        ? el.style.parsedOptions
                        : formats.time.medium;
                result.push({
                    type: PART_TYPE.literal,
                    value: formatters
                        .getDateTimeFormat(locales, style)
                        .format(value),
                });
                continue;
            }
            if (isNumberElement(el)) {
                var style = typeof el.style === 'string'
                    ? formats.number[el.style]
                    : isNumberSkeleton(el.style)
                        ? el.style.parsedOptions
                        : undefined;
                if (style && style.scale) {
                    value =
                        value *
                            (style.scale || 1);
                }
                result.push({
                    type: PART_TYPE.literal,
                    value: formatters
                        .getNumberFormat(locales, style)
                        .format(value),
                });
                continue;
            }
            if (isTagElement(el)) {
                var children = el.children, value_1 = el.value;
                var formatFn = values[value_1];
                if (!isFormatXMLElementFn(formatFn)) {
                    throw new InvalidValueTypeError(value_1, 'function', originalMessage);
                }
                var parts = formatToParts(children, locales, formatters, formats, values, currentPluralValue);
                var chunks = formatFn(parts.map(function (p) { return p.value; }));
                if (!Array.isArray(chunks)) {
                    chunks = [chunks];
                }
                result.push.apply(result, chunks.map(function (c) {
                    return {
                        type: typeof c === 'string' ? PART_TYPE.literal : PART_TYPE.object,
                        value: c,
                    };
                }));
            }
            if (isSelectElement(el)) {
                var opt = el.options[value] || el.options.other;
                if (!opt) {
                    throw new InvalidValueError(el.value, value, Object.keys(el.options), originalMessage);
                }
                result.push.apply(result, formatToParts(opt.value, locales, formatters, formats, values));
                continue;
            }
            if (isPluralElement(el)) {
                var opt = el.options["=".concat(value)];
                if (!opt) {
                    if (!Intl.PluralRules) {
                        throw new FormatError("Intl.PluralRules is not available in this environment.\nTry polyfilling it using \"@formatjs/intl-pluralrules\"\n", ErrorCode.MISSING_INTL_API, originalMessage);
                    }
                    var rule = formatters
                        .getPluralRules(locales, { type: el.pluralType })
                        .select(value - (el.offset || 0));
                    opt = el.options[rule] || el.options.other;
                }
                if (!opt) {
                    throw new InvalidValueError(el.value, value, Object.keys(el.options), originalMessage);
                }
                result.push.apply(result, formatToParts(opt.value, locales, formatters, formats, values, value - (el.offset || 0)));
                continue;
            }
        }
        return mergeLiteral(result);
    }

    /*
    Copyright (c) 2014, Yahoo! Inc. All rights reserved.
    Copyrights licensed under the New BSD License.
    See the accompanying LICENSE file for terms.
    */
    // -- MessageFormat --------------------------------------------------------
    function mergeConfig(c1, c2) {
        if (!c2) {
            return c1;
        }
        return __assign(__assign(__assign({}, (c1 || {})), (c2 || {})), Object.keys(c1).reduce(function (all, k) {
            all[k] = __assign(__assign({}, c1[k]), (c2[k] || {}));
            return all;
        }, {}));
    }
    function mergeConfigs(defaultConfig, configs) {
        if (!configs) {
            return defaultConfig;
        }
        return Object.keys(defaultConfig).reduce(function (all, k) {
            all[k] = mergeConfig(defaultConfig[k], configs[k]);
            return all;
        }, __assign({}, defaultConfig));
    }
    function createFastMemoizeCache(store) {
        return {
            create: function () {
                return {
                    get: function (key) {
                        return store[key];
                    },
                    set: function (key, value) {
                        store[key] = value;
                    },
                };
            },
        };
    }
    function createDefaultFormatters(cache) {
        if (cache === void 0) { cache = {
            number: {},
            dateTime: {},
            pluralRules: {},
        }; }
        return {
            getNumberFormat: memoize(function () {
                var _a;
                var args = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    args[_i] = arguments[_i];
                }
                return new ((_a = Intl.NumberFormat).bind.apply(_a, __spreadArray([void 0], args, false)))();
            }, {
                cache: createFastMemoizeCache(cache.number),
                strategy: strategies.variadic,
            }),
            getDateTimeFormat: memoize(function () {
                var _a;
                var args = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    args[_i] = arguments[_i];
                }
                return new ((_a = Intl.DateTimeFormat).bind.apply(_a, __spreadArray([void 0], args, false)))();
            }, {
                cache: createFastMemoizeCache(cache.dateTime),
                strategy: strategies.variadic,
            }),
            getPluralRules: memoize(function () {
                var _a;
                var args = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    args[_i] = arguments[_i];
                }
                return new ((_a = Intl.PluralRules).bind.apply(_a, __spreadArray([void 0], args, false)))();
            }, {
                cache: createFastMemoizeCache(cache.pluralRules),
                strategy: strategies.variadic,
            }),
        };
    }
    var IntlMessageFormat = /** @class */ (function () {
        function IntlMessageFormat(message, locales, overrideFormats, opts) {
            var _this = this;
            if (locales === void 0) { locales = IntlMessageFormat.defaultLocale; }
            this.formatterCache = {
                number: {},
                dateTime: {},
                pluralRules: {},
            };
            this.format = function (values) {
                var parts = _this.formatToParts(values);
                // Hot path for straight simple msg translations
                if (parts.length === 1) {
                    return parts[0].value;
                }
                var result = parts.reduce(function (all, part) {
                    if (!all.length ||
                        part.type !== PART_TYPE.literal ||
                        typeof all[all.length - 1] !== 'string') {
                        all.push(part.value);
                    }
                    else {
                        all[all.length - 1] += part.value;
                    }
                    return all;
                }, []);
                if (result.length <= 1) {
                    return result[0] || '';
                }
                return result;
            };
            this.formatToParts = function (values) {
                return formatToParts(_this.ast, _this.locales, _this.formatters, _this.formats, values, undefined, _this.message);
            };
            this.resolvedOptions = function () { return ({
                locale: _this.resolvedLocale.toString(),
            }); };
            this.getAst = function () { return _this.ast; };
            // Defined first because it's used to build the format pattern.
            this.locales = locales;
            this.resolvedLocale = IntlMessageFormat.resolveLocale(locales);
            if (typeof message === 'string') {
                this.message = message;
                if (!IntlMessageFormat.__parse) {
                    throw new TypeError('IntlMessageFormat.__parse must be set to process `message` of type `string`');
                }
                // Parse string messages into an AST.
                this.ast = IntlMessageFormat.__parse(message, {
                    ignoreTag: opts === null || opts === void 0 ? void 0 : opts.ignoreTag,
                    locale: this.resolvedLocale,
                });
            }
            else {
                this.ast = message;
            }
            if (!Array.isArray(this.ast)) {
                throw new TypeError('A message must be provided as a String or AST.');
            }
            // Creates a new object with the specified `formats` merged with the default
            // formats.
            this.formats = mergeConfigs(IntlMessageFormat.formats, overrideFormats);
            this.formatters =
                (opts && opts.formatters) || createDefaultFormatters(this.formatterCache);
        }
        Object.defineProperty(IntlMessageFormat, "defaultLocale", {
            get: function () {
                if (!IntlMessageFormat.memoizedDefaultLocale) {
                    IntlMessageFormat.memoizedDefaultLocale =
                        new Intl.NumberFormat().resolvedOptions().locale;
                }
                return IntlMessageFormat.memoizedDefaultLocale;
            },
            enumerable: false,
            configurable: true
        });
        IntlMessageFormat.memoizedDefaultLocale = null;
        IntlMessageFormat.resolveLocale = function (locales) {
            var supportedLocales = Intl.NumberFormat.supportedLocalesOf(locales);
            if (supportedLocales.length > 0) {
                return new Intl.Locale(supportedLocales[0]);
            }
            return new Intl.Locale(typeof locales === 'string' ? locales : locales[0]);
        };
        IntlMessageFormat.__parse = parse;
        // Default format options used as the prototype of the `formats` provided to the
        // constructor. These are used when constructing the internal Intl.NumberFormat
        // and Intl.DateTimeFormat instances.
        IntlMessageFormat.formats = {
            number: {
                integer: {
                    maximumFractionDigits: 0,
                },
                currency: {
                    style: 'currency',
                },
                percent: {
                    style: 'percent',
                },
            },
            date: {
                short: {
                    month: 'numeric',
                    day: 'numeric',
                    year: '2-digit',
                },
                medium: {
                    month: 'short',
                    day: 'numeric',
                    year: 'numeric',
                },
                long: {
                    month: 'long',
                    day: 'numeric',
                    year: 'numeric',
                },
                full: {
                    weekday: 'long',
                    month: 'long',
                    day: 'numeric',
                    year: 'numeric',
                },
            },
            time: {
                short: {
                    hour: 'numeric',
                    minute: 'numeric',
                },
                medium: {
                    hour: 'numeric',
                    minute: 'numeric',
                    second: 'numeric',
                },
                long: {
                    hour: 'numeric',
                    minute: 'numeric',
                    second: 'numeric',
                    timeZoneName: 'short',
                },
                full: {
                    hour: 'numeric',
                    minute: 'numeric',
                    second: 'numeric',
                    timeZoneName: 'short',
                },
            },
        };
        return IntlMessageFormat;
    }());

    function delve(obj, fullKey) {
      if (fullKey == null)
        return void 0;
      if (fullKey in obj) {
        return obj[fullKey];
      }
      const keys = fullKey.split(".");
      let result = obj;
      for (let p = 0; p < keys.length; p++) {
        if (typeof result === "object") {
          if (p > 0) {
            const partialKey = keys.slice(p, keys.length).join(".");
            if (partialKey in result) {
              result = result[partialKey];
              break;
            }
          }
          result = result[keys[p]];
        } else {
          result = void 0;
        }
      }
      return result;
    }

    const lookupCache = {};
    const addToCache = (path, locale, message) => {
      if (!message)
        return message;
      if (!(locale in lookupCache))
        lookupCache[locale] = {};
      if (!(path in lookupCache[locale]))
        lookupCache[locale][path] = message;
      return message;
    };
    const lookup = (path, refLocale) => {
      if (refLocale == null)
        return void 0;
      if (refLocale in lookupCache && path in lookupCache[refLocale]) {
        return lookupCache[refLocale][path];
      }
      const locales = getPossibleLocales(refLocale);
      for (let i = 0; i < locales.length; i++) {
        const locale = locales[i];
        const message = getMessageFromDictionary(locale, path);
        if (message) {
          return addToCache(path, refLocale, message);
        }
      }
      return void 0;
    };

    let dictionary;
    const $dictionary = writable({});
    function getLocaleDictionary(locale) {
      return dictionary[locale] || null;
    }
    function hasLocaleDictionary(locale) {
      return locale in dictionary;
    }
    function getMessageFromDictionary(locale, id) {
      if (!hasLocaleDictionary(locale)) {
        return null;
      }
      const localeDictionary = getLocaleDictionary(locale);
      const match = delve(localeDictionary, id);
      return match;
    }
    function getClosestAvailableLocale(refLocale) {
      if (refLocale == null)
        return void 0;
      const relatedLocales = getPossibleLocales(refLocale);
      for (let i = 0; i < relatedLocales.length; i++) {
        const locale = relatedLocales[i];
        if (hasLocaleDictionary(locale)) {
          return locale;
        }
      }
      return void 0;
    }
    function addMessages(locale, ...partials) {
      delete lookupCache[locale];
      $dictionary.update((d) => {
        d[locale] = cjs.all([d[locale] || {}, ...partials]);
        return d;
      });
    }
    derived(
      [$dictionary],
      ([dictionary2]) => Object.keys(dictionary2)
    );
    $dictionary.subscribe((newDictionary) => dictionary = newDictionary);

    const queue = {};
    function removeLoaderFromQueue(locale, loader) {
      queue[locale].delete(loader);
      if (queue[locale].size === 0) {
        delete queue[locale];
      }
    }
    function getLocaleQueue(locale) {
      return queue[locale];
    }
    function getLocalesQueues(locale) {
      return getPossibleLocales(locale).map((localeItem) => {
        const localeQueue = getLocaleQueue(localeItem);
        return [localeItem, localeQueue ? [...localeQueue] : []];
      }).filter(([, localeQueue]) => localeQueue.length > 0);
    }
    function hasLocaleQueue(locale) {
      if (locale == null)
        return false;
      return getPossibleLocales(locale).some(
        (localeQueue) => {
          var _a;
          return (_a = getLocaleQueue(localeQueue)) == null ? void 0 : _a.size;
        }
      );
    }
    function loadLocaleQueue(locale, localeQueue) {
      const allLoadersPromise = Promise.all(
        localeQueue.map((loader) => {
          removeLoaderFromQueue(locale, loader);
          return loader().then((partial) => partial.default || partial);
        })
      );
      return allLoadersPromise.then((partials) => addMessages(locale, ...partials));
    }
    const activeFlushes = {};
    function flush(locale) {
      if (!hasLocaleQueue(locale)) {
        if (locale in activeFlushes) {
          return activeFlushes[locale];
        }
        return Promise.resolve();
      }
      const queues = getLocalesQueues(locale);
      activeFlushes[locale] = Promise.all(
        queues.map(
          ([localeName, localeQueue]) => loadLocaleQueue(localeName, localeQueue)
        )
      ).then(() => {
        if (hasLocaleQueue(locale)) {
          return flush(locale);
        }
        delete activeFlushes[locale];
      });
      return activeFlushes[locale];
    }
    const defaultFormats = {
      number: {
        scientific: { notation: "scientific" },
        engineering: { notation: "engineering" },
        compactLong: { notation: "compact", compactDisplay: "long" },
        compactShort: { notation: "compact", compactDisplay: "short" }
      },
      date: {
        short: { month: "numeric", day: "numeric", year: "2-digit" },
        medium: { month: "short", day: "numeric", year: "numeric" },
        long: { month: "long", day: "numeric", year: "numeric" },
        full: { weekday: "long", month: "long", day: "numeric", year: "numeric" }
      },
      time: {
        short: { hour: "numeric", minute: "numeric" },
        medium: { hour: "numeric", minute: "numeric", second: "numeric" },
        long: {
          hour: "numeric",
          minute: "numeric",
          second: "numeric",
          timeZoneName: "short"
        },
        full: {
          hour: "numeric",
          minute: "numeric",
          second: "numeric",
          timeZoneName: "short"
        }
      }
    };
    const defaultOptions = {
      fallbackLocale: null,
      loadingDelay: 200,
      formats: defaultFormats,
      warnOnMissingMessages: true,
      handleMissingMessage: void 0,
      ignoreTag: true
    };
    const options = defaultOptions;
    function getOptions() {
      return options;
    }

    const $isLoading = writable(false);

    var __defProp$1 = Object.defineProperty;
    var __defProps = Object.defineProperties;
    var __getOwnPropDescs = Object.getOwnPropertyDescriptors;
    var __getOwnPropSymbols$1 = Object.getOwnPropertySymbols;
    var __hasOwnProp$1 = Object.prototype.hasOwnProperty;
    var __propIsEnum$1 = Object.prototype.propertyIsEnumerable;
    var __defNormalProp$1 = (obj, key, value) => key in obj ? __defProp$1(obj, key, { enumerable: true, configurable: true, writable: true, value }) : obj[key] = value;
    var __spreadValues$1 = (a, b) => {
      for (var prop in b || (b = {}))
        if (__hasOwnProp$1.call(b, prop))
          __defNormalProp$1(a, prop, b[prop]);
      if (__getOwnPropSymbols$1)
        for (var prop of __getOwnPropSymbols$1(b)) {
          if (__propIsEnum$1.call(b, prop))
            __defNormalProp$1(a, prop, b[prop]);
        }
      return a;
    };
    var __spreadProps = (a, b) => __defProps(a, __getOwnPropDescs(b));
    let current;
    const internalLocale = writable(null);
    function getSubLocales(refLocale) {
      return refLocale.split("-").map((_, i, arr) => arr.slice(0, i + 1).join("-")).reverse();
    }
    function getPossibleLocales(refLocale, fallbackLocale = getOptions().fallbackLocale) {
      const locales = getSubLocales(refLocale);
      if (fallbackLocale) {
        return [.../* @__PURE__ */ new Set([...locales, ...getSubLocales(fallbackLocale)])];
      }
      return locales;
    }
    function getCurrentLocale() {
      return current != null ? current : void 0;
    }
    internalLocale.subscribe((newLocale) => {
      current = newLocale != null ? newLocale : void 0;
      if (typeof window !== "undefined" && newLocale != null) {
        document.documentElement.setAttribute("lang", newLocale);
      }
    });
    const set = (newLocale) => {
      if (newLocale && getClosestAvailableLocale(newLocale) && hasLocaleQueue(newLocale)) {
        const { loadingDelay } = getOptions();
        let loadingTimer;
        if (typeof window !== "undefined" && getCurrentLocale() != null && loadingDelay) {
          loadingTimer = window.setTimeout(
            () => $isLoading.set(true),
            loadingDelay
          );
        } else {
          $isLoading.set(true);
        }
        return flush(newLocale).then(() => {
          internalLocale.set(newLocale);
        }).finally(() => {
          clearTimeout(loadingTimer);
          $isLoading.set(false);
        });
      }
      return internalLocale.set(newLocale);
    };
    const $locale = __spreadProps(__spreadValues$1({}, internalLocale), {
      set
    });

    const monadicMemoize = (fn) => {
      const cache = /* @__PURE__ */ Object.create(null);
      const memoizedFn = (arg) => {
        const cacheKey = JSON.stringify(arg);
        if (cacheKey in cache) {
          return cache[cacheKey];
        }
        return cache[cacheKey] = fn(arg);
      };
      return memoizedFn;
    };

    var __defProp = Object.defineProperty;
    var __getOwnPropSymbols = Object.getOwnPropertySymbols;
    var __hasOwnProp = Object.prototype.hasOwnProperty;
    var __propIsEnum = Object.prototype.propertyIsEnumerable;
    var __defNormalProp = (obj, key, value) => key in obj ? __defProp(obj, key, { enumerable: true, configurable: true, writable: true, value }) : obj[key] = value;
    var __spreadValues = (a, b) => {
      for (var prop in b || (b = {}))
        if (__hasOwnProp.call(b, prop))
          __defNormalProp(a, prop, b[prop]);
      if (__getOwnPropSymbols)
        for (var prop of __getOwnPropSymbols(b)) {
          if (__propIsEnum.call(b, prop))
            __defNormalProp(a, prop, b[prop]);
        }
      return a;
    };
    var __objRest = (source, exclude) => {
      var target = {};
      for (var prop in source)
        if (__hasOwnProp.call(source, prop) && exclude.indexOf(prop) < 0)
          target[prop] = source[prop];
      if (source != null && __getOwnPropSymbols)
        for (var prop of __getOwnPropSymbols(source)) {
          if (exclude.indexOf(prop) < 0 && __propIsEnum.call(source, prop))
            target[prop] = source[prop];
        }
      return target;
    };
    const getIntlFormatterOptions = (type, name) => {
      const { formats } = getOptions();
      if (type in formats && name in formats[type]) {
        return formats[type][name];
      }
      throw new Error(`[svelte-i18n] Unknown "${name}" ${type} format.`);
    };
    const createNumberFormatter = monadicMemoize(
      (_a) => {
        var _b = _a, { locale, format } = _b, options = __objRest(_b, ["locale", "format"]);
        if (locale == null) {
          throw new Error('[svelte-i18n] A "locale" must be set to format numbers');
        }
        if (format) {
          options = getIntlFormatterOptions("number", format);
        }
        return new Intl.NumberFormat(locale, options);
      }
    );
    const createDateFormatter = monadicMemoize(
      (_c) => {
        var _d = _c, { locale, format } = _d, options = __objRest(_d, ["locale", "format"]);
        if (locale == null) {
          throw new Error('[svelte-i18n] A "locale" must be set to format dates');
        }
        if (format) {
          options = getIntlFormatterOptions("date", format);
        } else if (Object.keys(options).length === 0) {
          options = getIntlFormatterOptions("date", "short");
        }
        return new Intl.DateTimeFormat(locale, options);
      }
    );
    const createTimeFormatter = monadicMemoize(
      (_e) => {
        var _f = _e, { locale, format } = _f, options = __objRest(_f, ["locale", "format"]);
        if (locale == null) {
          throw new Error(
            '[svelte-i18n] A "locale" must be set to format time values'
          );
        }
        if (format) {
          options = getIntlFormatterOptions("time", format);
        } else if (Object.keys(options).length === 0) {
          options = getIntlFormatterOptions("time", "short");
        }
        return new Intl.DateTimeFormat(locale, options);
      }
    );
    const getNumberFormatter = (_g = {}) => {
      var _h = _g, {
        locale = getCurrentLocale()
      } = _h, args = __objRest(_h, [
        "locale"
      ]);
      return createNumberFormatter(__spreadValues({ locale }, args));
    };
    const getDateFormatter = (_i = {}) => {
      var _j = _i, {
        locale = getCurrentLocale()
      } = _j, args = __objRest(_j, [
        "locale"
      ]);
      return createDateFormatter(__spreadValues({ locale }, args));
    };
    const getTimeFormatter = (_k = {}) => {
      var _l = _k, {
        locale = getCurrentLocale()
      } = _l, args = __objRest(_l, [
        "locale"
      ]);
      return createTimeFormatter(__spreadValues({ locale }, args));
    };
    const getMessageFormatter = monadicMemoize(
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      (message, locale = getCurrentLocale()) => new IntlMessageFormat(message, locale, getOptions().formats, {
        ignoreTag: getOptions().ignoreTag
      })
    );

    const formatMessage = (id, options = {}) => {
      var _a, _b, _c, _d;
      let messageObj = options;
      if (typeof id === "object") {
        messageObj = id;
        id = messageObj.id;
      }
      const {
        values,
        locale = getCurrentLocale(),
        default: defaultValue
      } = messageObj;
      if (locale == null) {
        throw new Error(
          "[svelte-i18n] Cannot format a message without first setting the initial locale."
        );
      }
      let message = lookup(id, locale);
      if (!message) {
        message = (_d = (_c = (_b = (_a = getOptions()).handleMissingMessage) == null ? void 0 : _b.call(_a, { locale, id, defaultValue })) != null ? _c : defaultValue) != null ? _d : id;
      } else if (typeof message !== "string") {
        console.warn(
          `[svelte-i18n] Message with id "${id}" must be of type "string", found: "${typeof message}". Gettin its value through the "$format" method is deprecated; use the "json" method instead.`
        );
        return message;
      }
      if (!values) {
        return message;
      }
      let result = message;
      try {
        result = getMessageFormatter(message, locale).format(values);
      } catch (e) {
        if (e instanceof Error) {
          console.warn(
            `[svelte-i18n] Message "${id}" has syntax error:`,
            e.message
          );
        }
      }
      return result;
    };
    const formatTime = (t, options) => {
      return getTimeFormatter(options).format(t);
    };
    const formatDate = (d, options) => {
      return getDateFormatter(options).format(d);
    };
    const formatNumber = (n, options) => {
      return getNumberFormatter(options).format(n);
    };
    const getJSON = (id, locale = getCurrentLocale()) => {
      return lookup(id, locale);
    };
    const $format = derived([$locale, $dictionary], () => formatMessage);
    derived([$locale], () => formatTime);
    derived([$locale], () => formatDate);
    derived([$locale], () => formatNumber);
    derived([$locale, $dictionary], () => getJSON);

    const MESSAGE_FILE_URL_TEMPLATE = '/lang/{locale}.json';

    function setupI18n({ withLocale: _locale } = { withLocale: 'de' }) {
        const messsagesFileUrl = MESSAGE_FILE_URL_TEMPLATE.replace('{locale}', _locale);

        return fetch(messsagesFileUrl)
            .then(response => response.json())
            .then((messages) => {
                $dictionary.set({ [_locale]: messages });

                $locale.set(_locale);
            });
    }

    const isLocaleLoaded = derived($locale, $locale => typeof $locale === 'string');
    const localeDescription = derived($locale, $locale => $locale === "en" ? "English" : $locale === "nl" ? "Nederlands" : $locale === "rs" ? "Srpski" : "Deutsch");

    /* src\Home.svelte generated by Svelte v3.59.2 */
    const file$b = "src\\Home.svelte";

    // (15:0) {:else}
    function create_else_block$4(ctx) {
    	let button;
    	let t_value = /*$_*/ ctx[0]("login") + "";
    	let t;
    	let mounted;
    	let dispose;

    	const block = {
    		c: function create() {
    			button = element("button");
    			t = text(t_value);
    			add_location(button, file$b, 15, 4, 292);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, button, anchor);
    			append_dev(button, t);

    			if (!mounted) {
    				dispose = listen_dev(button, "click", /*goLoginPage*/ ctx[2], false, false, false, false);
    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*$_*/ 1 && t_value !== (t_value = /*$_*/ ctx[0]("login") + "")) set_data_dev(t, t_value);
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(button);
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_else_block$4.name,
    		type: "else",
    		source: "(15:0) {:else}",
    		ctx
    	});

    	return block;
    }

    // (13:0) {#if $s.loggedIn}
    function create_if_block$6(ctx) {
    	let h4;
    	let t_value = /*$s*/ ctx[1].userName + "";
    	let t;

    	const block = {
    		c: function create() {
    			h4 = element("h4");
    			t = text(t_value);
    			add_location(h4, file$b, 13, 4, 257);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, h4, anchor);
    			append_dev(h4, t);
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*$s*/ 2 && t_value !== (t_value = /*$s*/ ctx[1].userName + "")) set_data_dev(t, t_value);
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(h4);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block$6.name,
    		type: "if",
    		source: "(13:0) {#if $s.loggedIn}",
    		ctx
    	});

    	return block;
    }

    function create_fragment$b(ctx) {
    	let h3;
    	let t0_value = /*$_*/ ctx[0]("welcome") + "";
    	let t0;
    	let t1;
    	let if_block_anchor;

    	function select_block_type(ctx, dirty) {
    		if (/*$s*/ ctx[1].loggedIn) return create_if_block$6;
    		return create_else_block$4;
    	}

    	let current_block_type = select_block_type(ctx);
    	let if_block = current_block_type(ctx);

    	const block = {
    		c: function create() {
    			h3 = element("h3");
    			t0 = text(t0_value);
    			t1 = space();
    			if_block.c();
    			if_block_anchor = empty();
    			add_location(h3, file$b, 10, 0, 209);
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, h3, anchor);
    			append_dev(h3, t0);
    			insert_dev(target, t1, anchor);
    			if_block.m(target, anchor);
    			insert_dev(target, if_block_anchor, anchor);
    		},
    		p: function update(ctx, [dirty]) {
    			if (dirty & /*$_*/ 1 && t0_value !== (t0_value = /*$_*/ ctx[0]("welcome") + "")) set_data_dev(t0, t0_value);

    			if (current_block_type === (current_block_type = select_block_type(ctx)) && if_block) {
    				if_block.p(ctx, dirty);
    			} else {
    				if_block.d(1);
    				if_block = current_block_type(ctx);

    				if (if_block) {
    					if_block.c();
    					if_block.m(if_block_anchor.parentNode, if_block_anchor);
    				}
    			}
    		},
    		i: noop,
    		o: noop,
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(h3);
    			if (detaching) detach_dev(t1);
    			if_block.d(detaching);
    			if (detaching) detach_dev(if_block_anchor);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$b.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function instance$b($$self, $$props, $$invalidate) {
    	let $_;
    	let $s;
    	validate_store($format, '_');
    	component_subscribe($$self, $format, $$value => $$invalidate(0, $_ = $$value));
    	validate_store(s, 's');
    	component_subscribe($$self, s, $$value => $$invalidate(1, $s = $$value));
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Home', slots, []);

    	function goLoginPage() {
    		push("/login");
    	}

    	const writable_props = [];

    	Object.keys($$props).forEach(key => {
    		if (!~writable_props.indexOf(key) && key.slice(0, 2) !== '$$' && key !== 'slot') console.warn(`<Home> was created with unknown prop '${key}'`);
    	});

    	$$self.$capture_state = () => ({ _: $format, push, s, goLoginPage, $_, $s });
    	return [$_, $s, goLoginPage];
    }

    class Home extends SvelteComponentDev {
    	constructor(options) {
    		super(options);
    		init(this, options, instance$b, create_fragment$b, safe_not_equal, {});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Home",
    			options,
    			id: create_fragment$b.name
    		});
    	}
    }

    /* src\Workouts.svelte generated by Svelte v3.59.2 */
    const file$a = "src\\Workouts.svelte";

    function create_fragment$a(ctx) {
    	let h3;
    	let t_value = /*$_*/ ctx[0]("workouts") + "";
    	let t;

    	const block = {
    		c: function create() {
    			h3 = element("h3");
    			t = text(t_value);
    			add_location(h3, file$a, 6, 0, 149);
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, h3, anchor);
    			append_dev(h3, t);
    		},
    		p: function update(ctx, [dirty]) {
    			if (dirty & /*$_*/ 1 && t_value !== (t_value = /*$_*/ ctx[0]("workouts") + "")) set_data_dev(t, t_value);
    		},
    		i: noop,
    		o: noop,
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(h3);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$a.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function instance$a($$self, $$props, $$invalidate) {
    	let $_;
    	validate_store($format, '_');
    	component_subscribe($$self, $format, $$value => $$invalidate(0, $_ = $$value));
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Workouts', slots, []);
    	const writable_props = [];

    	Object.keys($$props).forEach(key => {
    		if (!~writable_props.indexOf(key) && key.slice(0, 2) !== '$$' && key !== 'slot') console.warn(`<Workouts> was created with unknown prop '${key}'`);
    	});

    	$$self.$capture_state = () => ({ _: $format, push, s, $_ });
    	return [$_];
    }

    class Workouts extends SvelteComponentDev {
    	constructor(options) {
    		super(options);
    		init(this, options, instance$a, create_fragment$a, safe_not_equal, {});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Workouts",
    			options,
    			id: create_fragment$a.name
    		});
    	}
    }

    const api = "/api/v1/index.prg";
    const url = "" + api;

    const APIGet = async (request) => {

        try {
            const res = await fetch(url, {
                method: "POST",
                mode: "cors",
                headers: {
                    "Content-Type": "application/json"
                },
                body: JSON.stringify(request)
            });
            let r = await res.json();
            return Promise.resolve(r);
        } catch (err) {
            return Promise.reject(err);
        }

    };

    /* src\Login.svelte generated by Svelte v3.59.2 */

    const { console: console_1$1 } = globals;
    const file$9 = "src\\Login.svelte";

    // (80:0) {#if e.length > 0}
    function create_if_block_1$3(ctx) {
    	let p_1;
    	let t;

    	const block = {
    		c: function create() {
    			p_1 = element("p");
    			t = text(/*e*/ ctx[2]);
    			add_location(p_1, file$9, 80, 4, 1701);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, p_1, anchor);
    			append_dev(p_1, t);
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*e*/ 4) set_data_dev(t, /*e*/ ctx[2]);
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(p_1);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block_1$3.name,
    		type: "if",
    		source: "(80:0) {#if e.length > 0}",
    		ctx
    	});

    	return block;
    }

    // (98:0) {:else}
    function create_else_block$3(ctx) {
    	let form0;
    	let input0;
    	let input0_placeholder_value;
    	let t0;
    	let input1;
    	let input1_placeholder_value;
    	let t1;
    	let button0;
    	let t2_value = /*$_*/ ctx[6]("login") + "";
    	let t2;
    	let t3;
    	let br;
    	let t4;
    	let form1;
    	let p_1;
    	let t5_value = /*$_*/ ctx[6]("no_account_register") + "";
    	let t5;
    	let t6;
    	let button1;
    	let t7_value = /*$_*/ ctx[6]("register") + "";
    	let t7;
    	let mounted;
    	let dispose;

    	const block = {
    		c: function create() {
    			form0 = element("form");
    			input0 = element("input");
    			t0 = space();
    			input1 = element("input");
    			t1 = space();
    			button0 = element("button");
    			t2 = text(t2_value);
    			t3 = space();
    			br = element("br");
    			t4 = space();
    			form1 = element("form");
    			p_1 = element("p");
    			t5 = text(t5_value);
    			t6 = space();
    			button1 = element("button");
    			t7 = text(t7_value);
    			attr_dev(input0, "placeholder", input0_placeholder_value = /*$_*/ ctx[6]("user"));
    			add_location(input0, file$9, 99, 8, 2444);
    			attr_dev(input1, "placeholder", input1_placeholder_value = /*$_*/ ctx[6]("password"));
    			add_location(input1, file$9, 100, 8, 2502);
    			attr_dev(button0, "type", "submit");
    			add_location(button0, file$9, 101, 8, 2564);
    			add_location(form0, file$9, 98, 4, 2393);
    			add_location(br, file$9, 103, 4, 2625);
    			add_location(p_1, file$9, 105, 8, 2691);
    			attr_dev(button1, "type", "submit");
    			add_location(button1, file$9, 106, 8, 2734);
    			add_location(form1, file$9, 104, 4, 2636);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, form0, anchor);
    			append_dev(form0, input0);
    			set_input_value(input0, /*u*/ ctx[0]);
    			append_dev(form0, t0);
    			append_dev(form0, input1);
    			set_input_value(input1, /*p*/ ctx[1]);
    			append_dev(form0, t1);
    			append_dev(form0, button0);
    			append_dev(button0, t2);
    			insert_dev(target, t3, anchor);
    			insert_dev(target, br, anchor);
    			insert_dev(target, t4, anchor);
    			insert_dev(target, form1, anchor);
    			append_dev(form1, p_1);
    			append_dev(p_1, t5);
    			append_dev(form1, t6);
    			append_dev(form1, button1);
    			append_dev(button1, t7);

    			if (!mounted) {
    				dispose = [
    					listen_dev(input0, "input", /*input0_input_handler_1*/ ctx[16]),
    					listen_dev(input1, "input", /*input1_input_handler_1*/ ctx[17]),
    					listen_dev(form0, "submit", prevent_default(/*tryLogin*/ ctx[7]), false, true, false, false),
    					listen_dev(form1, "submit", prevent_default(/*showRegister*/ ctx[9]), false, true, false, false)
    				];

    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*$_*/ 64 && input0_placeholder_value !== (input0_placeholder_value = /*$_*/ ctx[6]("user"))) {
    				attr_dev(input0, "placeholder", input0_placeholder_value);
    			}

    			if (dirty & /*u*/ 1 && input0.value !== /*u*/ ctx[0]) {
    				set_input_value(input0, /*u*/ ctx[0]);
    			}

    			if (dirty & /*$_*/ 64 && input1_placeholder_value !== (input1_placeholder_value = /*$_*/ ctx[6]("password"))) {
    				attr_dev(input1, "placeholder", input1_placeholder_value);
    			}

    			if (dirty & /*p*/ 2 && input1.value !== /*p*/ ctx[1]) {
    				set_input_value(input1, /*p*/ ctx[1]);
    			}

    			if (dirty & /*$_*/ 64 && t2_value !== (t2_value = /*$_*/ ctx[6]("login") + "")) set_data_dev(t2, t2_value);
    			if (dirty & /*$_*/ 64 && t5_value !== (t5_value = /*$_*/ ctx[6]("no_account_register") + "")) set_data_dev(t5, t5_value);
    			if (dirty & /*$_*/ 64 && t7_value !== (t7_value = /*$_*/ ctx[6]("register") + "")) set_data_dev(t7, t7_value);
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(form0);
    			if (detaching) detach_dev(t3);
    			if (detaching) detach_dev(br);
    			if (detaching) detach_dev(t4);
    			if (detaching) detach_dev(form1);
    			mounted = false;
    			run_all(dispose);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_else_block$3.name,
    		type: "else",
    		source: "(98:0) {:else}",
    		ctx
    	});

    	return block;
    }

    // (84:0) {#if register}
    function create_if_block$5(ctx) {
    	let form0;
    	let input0;
    	let input0_placeholder_value;
    	let t0;
    	let input1;
    	let input1_placeholder_value;
    	let t1;
    	let input2;
    	let input2_placeholder_value;
    	let t2;
    	let input3;
    	let input3_placeholder_value;
    	let t3;
    	let input4;
    	let input4_placeholder_value;
    	let t4;
    	let button0;
    	let t5_value = /*$_*/ ctx[6]("register") + "";
    	let t5;
    	let t6;
    	let br;
    	let t7;
    	let form1;
    	let p_1;
    	let t8_value = /*$_*/ ctx[6]("you_have_account") + "";
    	let t8;
    	let t9;
    	let button1;
    	let t10_value = /*$_*/ ctx[6]("login") + "";
    	let t10;
    	let mounted;
    	let dispose;

    	const block = {
    		c: function create() {
    			form0 = element("form");
    			input0 = element("input");
    			t0 = space();
    			input1 = element("input");
    			t1 = space();
    			input2 = element("input");
    			t2 = space();
    			input3 = element("input");
    			t3 = space();
    			input4 = element("input");
    			t4 = space();
    			button0 = element("button");
    			t5 = text(t5_value);
    			t6 = space();
    			br = element("br");
    			t7 = space();
    			form1 = element("form");
    			p_1 = element("p");
    			t8 = text(t8_value);
    			t9 = space();
    			button1 = element("button");
    			t10 = text(t10_value);
    			attr_dev(input0, "placeholder", input0_placeholder_value = /*$_*/ ctx[6]("user"));
    			add_location(input0, file$9, 85, 8, 1792);
    			attr_dev(input1, "placeholder", input1_placeholder_value = /*$_*/ ctx[6]("first_name"));
    			add_location(input1, file$9, 86, 8, 1859);
    			attr_dev(input2, "placeholder", input2_placeholder_value = /*$_*/ ctx[6]("last_name"));
    			add_location(input2, file$9, 87, 8, 1937);
    			attr_dev(input3, "placeholder", input3_placeholder_value = /*$_*/ ctx[6]("email"));
    			add_location(input3, file$9, 88, 8, 2013);
    			attr_dev(input4, "placeholder", input4_placeholder_value = /*$_*/ ctx[6]("password"));
    			add_location(input4, file$9, 89, 8, 2082);
    			attr_dev(button0, "type", "submit");
    			add_location(button0, file$9, 90, 8, 2157);
    			add_location(form0, file$9, 84, 4, 1738);
    			add_location(br, file$9, 92, 4, 2221);
    			add_location(p_1, file$9, 94, 8, 2284);
    			attr_dev(button1, "type", "submit");
    			add_location(button1, file$9, 95, 8, 2324);
    			add_location(form1, file$9, 93, 4, 2232);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, form0, anchor);
    			append_dev(form0, input0);
    			set_input_value(input0, /*oUser*/ ctx[4].nick);
    			append_dev(form0, t0);
    			append_dev(form0, input1);
    			set_input_value(input1, /*oUser*/ ctx[4].firstname);
    			append_dev(form0, t1);
    			append_dev(form0, input2);
    			set_input_value(input2, /*oUser*/ ctx[4].lastname);
    			append_dev(form0, t2);
    			append_dev(form0, input3);
    			set_input_value(input3, /*oUser*/ ctx[4].email);
    			append_dev(form0, t3);
    			append_dev(form0, input4);
    			set_input_value(input4, /*oUser*/ ctx[4].password);
    			append_dev(form0, t4);
    			append_dev(form0, button0);
    			append_dev(button0, t5);
    			insert_dev(target, t6, anchor);
    			insert_dev(target, br, anchor);
    			insert_dev(target, t7, anchor);
    			insert_dev(target, form1, anchor);
    			append_dev(form1, p_1);
    			append_dev(p_1, t8);
    			append_dev(form1, t9);
    			append_dev(form1, button1);
    			append_dev(button1, t10);

    			if (!mounted) {
    				dispose = [
    					listen_dev(input0, "input", /*input0_input_handler*/ ctx[11]),
    					listen_dev(input1, "input", /*input1_input_handler*/ ctx[12]),
    					listen_dev(input2, "input", /*input2_input_handler*/ ctx[13]),
    					listen_dev(input3, "input", /*input3_input_handler*/ ctx[14]),
    					listen_dev(input4, "input", /*input4_input_handler*/ ctx[15]),
    					listen_dev(form0, "submit", prevent_default(/*tryRegister*/ ctx[8]), false, true, false, false),
    					listen_dev(form1, "submit", prevent_default(/*showLogin*/ ctx[10]), false, true, false, false)
    				];

    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*$_*/ 64 && input0_placeholder_value !== (input0_placeholder_value = /*$_*/ ctx[6]("user"))) {
    				attr_dev(input0, "placeholder", input0_placeholder_value);
    			}

    			if (dirty & /*oUser*/ 16 && input0.value !== /*oUser*/ ctx[4].nick) {
    				set_input_value(input0, /*oUser*/ ctx[4].nick);
    			}

    			if (dirty & /*$_*/ 64 && input1_placeholder_value !== (input1_placeholder_value = /*$_*/ ctx[6]("first_name"))) {
    				attr_dev(input1, "placeholder", input1_placeholder_value);
    			}

    			if (dirty & /*oUser*/ 16 && input1.value !== /*oUser*/ ctx[4].firstname) {
    				set_input_value(input1, /*oUser*/ ctx[4].firstname);
    			}

    			if (dirty & /*$_*/ 64 && input2_placeholder_value !== (input2_placeholder_value = /*$_*/ ctx[6]("last_name"))) {
    				attr_dev(input2, "placeholder", input2_placeholder_value);
    			}

    			if (dirty & /*oUser*/ 16 && input2.value !== /*oUser*/ ctx[4].lastname) {
    				set_input_value(input2, /*oUser*/ ctx[4].lastname);
    			}

    			if (dirty & /*$_*/ 64 && input3_placeholder_value !== (input3_placeholder_value = /*$_*/ ctx[6]("email"))) {
    				attr_dev(input3, "placeholder", input3_placeholder_value);
    			}

    			if (dirty & /*oUser*/ 16 && input3.value !== /*oUser*/ ctx[4].email) {
    				set_input_value(input3, /*oUser*/ ctx[4].email);
    			}

    			if (dirty & /*$_*/ 64 && input4_placeholder_value !== (input4_placeholder_value = /*$_*/ ctx[6]("password"))) {
    				attr_dev(input4, "placeholder", input4_placeholder_value);
    			}

    			if (dirty & /*oUser*/ 16 && input4.value !== /*oUser*/ ctx[4].password) {
    				set_input_value(input4, /*oUser*/ ctx[4].password);
    			}

    			if (dirty & /*$_*/ 64 && t5_value !== (t5_value = /*$_*/ ctx[6]("register") + "")) set_data_dev(t5, t5_value);
    			if (dirty & /*$_*/ 64 && t8_value !== (t8_value = /*$_*/ ctx[6]("you_have_account") + "")) set_data_dev(t8, t8_value);
    			if (dirty & /*$_*/ 64 && t10_value !== (t10_value = /*$_*/ ctx[6]("login") + "")) set_data_dev(t10, t10_value);
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(form0);
    			if (detaching) detach_dev(t6);
    			if (detaching) detach_dev(br);
    			if (detaching) detach_dev(t7);
    			if (detaching) detach_dev(form1);
    			mounted = false;
    			run_all(dispose);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block$5.name,
    		type: "if",
    		source: "(84:0) {#if register}",
    		ctx
    	});

    	return block;
    }

    function create_fragment$9(ctx) {
    	let h10;
    	let t0_value = /*$_*/ ctx[6]("login") + "";
    	let t0;
    	let t1;
    	let h11;
    	let t2_value = /*$s*/ ctx[5].userName + "";
    	let t2;
    	let t3;
    	let t4;
    	let if_block1_anchor;
    	let if_block0 = /*e*/ ctx[2].length > 0 && create_if_block_1$3(ctx);

    	function select_block_type(ctx, dirty) {
    		if (/*register*/ ctx[3]) return create_if_block$5;
    		return create_else_block$3;
    	}

    	let current_block_type = select_block_type(ctx);
    	let if_block1 = current_block_type(ctx);

    	const block = {
    		c: function create() {
    			h10 = element("h1");
    			t0 = text(t0_value);
    			t1 = space();
    			h11 = element("h1");
    			t2 = text(t2_value);
    			t3 = space();
    			if (if_block0) if_block0.c();
    			t4 = space();
    			if_block1.c();
    			if_block1_anchor = empty();
    			add_location(h10, file$9, 75, 0, 1630);
    			add_location(h11, file$9, 77, 0, 1654);
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, h10, anchor);
    			append_dev(h10, t0);
    			insert_dev(target, t1, anchor);
    			insert_dev(target, h11, anchor);
    			append_dev(h11, t2);
    			insert_dev(target, t3, anchor);
    			if (if_block0) if_block0.m(target, anchor);
    			insert_dev(target, t4, anchor);
    			if_block1.m(target, anchor);
    			insert_dev(target, if_block1_anchor, anchor);
    		},
    		p: function update(ctx, [dirty]) {
    			if (dirty & /*$_*/ 64 && t0_value !== (t0_value = /*$_*/ ctx[6]("login") + "")) set_data_dev(t0, t0_value);
    			if (dirty & /*$s*/ 32 && t2_value !== (t2_value = /*$s*/ ctx[5].userName + "")) set_data_dev(t2, t2_value);

    			if (/*e*/ ctx[2].length > 0) {
    				if (if_block0) {
    					if_block0.p(ctx, dirty);
    				} else {
    					if_block0 = create_if_block_1$3(ctx);
    					if_block0.c();
    					if_block0.m(t4.parentNode, t4);
    				}
    			} else if (if_block0) {
    				if_block0.d(1);
    				if_block0 = null;
    			}

    			if (current_block_type === (current_block_type = select_block_type(ctx)) && if_block1) {
    				if_block1.p(ctx, dirty);
    			} else {
    				if_block1.d(1);
    				if_block1 = current_block_type(ctx);

    				if (if_block1) {
    					if_block1.c();
    					if_block1.m(if_block1_anchor.parentNode, if_block1_anchor);
    				}
    			}
    		},
    		i: noop,
    		o: noop,
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(h10);
    			if (detaching) detach_dev(t1);
    			if (detaching) detach_dev(h11);
    			if (detaching) detach_dev(t3);
    			if (if_block0) if_block0.d(detaching);
    			if (detaching) detach_dev(t4);
    			if_block1.d(detaching);
    			if (detaching) detach_dev(if_block1_anchor);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$9.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function instance$9($$self, $$props, $$invalidate) {
    	let $s;
    	let $_;
    	validate_store(s, 's');
    	component_subscribe($$self, s, $$value => $$invalidate(5, $s = $$value));
    	validate_store($format, '_');
    	component_subscribe($$self, $format, $$value => $$invalidate(6, $_ = $$value));
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Login', slots, []);
    	let u = "";
    	let p = "";
    	let e = "";
    	let register = false;

    	let oUser = {
    		method: "register",
    		nick: "",
    		firstname: "",
    		lastname: "",
    		email: "",
    		password: ""
    	};

    	async function tryLogin() {
    		const user = u, password = p;
    		$$invalidate(0, u = "");
    		$$invalidate(1, p = "");
    		$$invalidate(2, e = "");
    		const res = await APIGet({ method: "login", user, password });
    		console.log(res);

    		if (res.success) {
    			set_store_value(s, $s.userId = res.nick, $s);
    			set_store_value(s, $s.userName = res.firstname + " " + res.lastname, $s);
    			set_store_value(s, $s.loggedIn = true, $s);
    			push("/");
    			return;
    		} else {
    			set_store_value(s, $s.userId = "", $s);
    			set_store_value(s, $s.userName = "", $s);
    			set_store_value(s, $s.loggedIn = false, $s);
    		}

    		$$invalidate(2, e = res.error);
    	}

    	async function tryRegister() {
    		const res = await APIGet(oUser);
    		console.log(res);

    		if (res.success) {
    			set_store_value(s, $s.userId = res.nick, $s);
    			set_store_value(s, $s.userName = res.firstname + " " + res.lastname, $s);
    			set_store_value(s, $s.loggedIn = true, $s);
    			push("/");
    			return;
    		} else {
    			set_store_value(s, $s.userId = "", $s);
    			set_store_value(s, $s.userName = "", $s);
    			set_store_value(s, $s.loggedIn = false, $s);
    		}

    		$$invalidate(2, e = res.error);
    	}

    	function showRegister() {
    		$$invalidate(3, register = true);
    	}

    	function showLogin() {
    		$$invalidate(3, register = false);
    	}

    	const writable_props = [];

    	Object.keys($$props).forEach(key => {
    		if (!~writable_props.indexOf(key) && key.slice(0, 2) !== '$$' && key !== 'slot') console_1$1.warn(`<Login> was created with unknown prop '${key}'`);
    	});

    	function input0_input_handler() {
    		oUser.nick = this.value;
    		$$invalidate(4, oUser);
    	}

    	function input1_input_handler() {
    		oUser.firstname = this.value;
    		$$invalidate(4, oUser);
    	}

    	function input2_input_handler() {
    		oUser.lastname = this.value;
    		$$invalidate(4, oUser);
    	}

    	function input3_input_handler() {
    		oUser.email = this.value;
    		$$invalidate(4, oUser);
    	}

    	function input4_input_handler() {
    		oUser.password = this.value;
    		$$invalidate(4, oUser);
    	}

    	function input0_input_handler_1() {
    		u = this.value;
    		$$invalidate(0, u);
    	}

    	function input1_input_handler_1() {
    		p = this.value;
    		$$invalidate(1, p);
    	}

    	$$self.$capture_state = () => ({
    		_: $format,
    		s,
    		APIGet,
    		push,
    		u,
    		p,
    		e,
    		register,
    		oUser,
    		tryLogin,
    		tryRegister,
    		showRegister,
    		showLogin,
    		$s,
    		$_
    	});

    	$$self.$inject_state = $$props => {
    		if ('u' in $$props) $$invalidate(0, u = $$props.u);
    		if ('p' in $$props) $$invalidate(1, p = $$props.p);
    		if ('e' in $$props) $$invalidate(2, e = $$props.e);
    		if ('register' in $$props) $$invalidate(3, register = $$props.register);
    		if ('oUser' in $$props) $$invalidate(4, oUser = $$props.oUser);
    	};

    	if ($$props && "$$inject" in $$props) {
    		$$self.$inject_state($$props.$$inject);
    	}

    	return [
    		u,
    		p,
    		e,
    		register,
    		oUser,
    		$s,
    		$_,
    		tryLogin,
    		tryRegister,
    		showRegister,
    		showLogin,
    		input0_input_handler,
    		input1_input_handler,
    		input2_input_handler,
    		input3_input_handler,
    		input4_input_handler,
    		input0_input_handler_1,
    		input1_input_handler_1
    	];
    }

    class Login extends SvelteComponentDev {
    	constructor(options) {
    		super(options);
    		init(this, options, instance$9, create_fragment$9, safe_not_equal, {});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Login",
    			options,
    			id: create_fragment$9.name
    		});
    	}
    }

    /* src\Logout.svelte generated by Svelte v3.59.2 */

    const { console: console_1 } = globals;
    const file$8 = "src\\Logout.svelte";

    // (28:0) {#if e.length > 0}
    function create_if_block$4(ctx) {
    	let p;
    	let t;

    	const block = {
    		c: function create() {
    			p = element("p");
    			t = text(/*e*/ ctx[0]);
    			add_location(p, file$8, 28, 4, 593);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, p, anchor);
    			append_dev(p, t);
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*e*/ 1) set_data_dev(t, /*e*/ ctx[0]);
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(p);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block$4.name,
    		type: "if",
    		source: "(28:0) {#if e.length > 0}",
    		ctx
    	});

    	return block;
    }

    function create_fragment$8(ctx) {
    	let h1;
    	let t0_value = /*$_*/ ctx[1]("logout") + "";
    	let t0;
    	let t1;
    	let t2;
    	let form;
    	let button;
    	let t3_value = /*$_*/ ctx[1]("logout") + "";
    	let t3;
    	let mounted;
    	let dispose;
    	let if_block = /*e*/ ctx[0].length > 0 && create_if_block$4(ctx);

    	const block = {
    		c: function create() {
    			h1 = element("h1");
    			t0 = text(t0_value);
    			t1 = space();
    			if (if_block) if_block.c();
    			t2 = space();
    			form = element("form");
    			button = element("button");
    			t3 = text(t3_value);
    			add_location(h1, file$8, 25, 0, 545);
    			attr_dev(button, "type", "submit");
    			add_location(button, file$8, 32, 4, 659);
    			add_location(form, file$8, 31, 0, 611);
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, h1, anchor);
    			append_dev(h1, t0);
    			insert_dev(target, t1, anchor);
    			if (if_block) if_block.m(target, anchor);
    			insert_dev(target, t2, anchor);
    			insert_dev(target, form, anchor);
    			append_dev(form, button);
    			append_dev(button, t3);

    			if (!mounted) {
    				dispose = listen_dev(form, "submit", prevent_default(/*tryLogout*/ ctx[2]), false, true, false, false);
    				mounted = true;
    			}
    		},
    		p: function update(ctx, [dirty]) {
    			if (dirty & /*$_*/ 2 && t0_value !== (t0_value = /*$_*/ ctx[1]("logout") + "")) set_data_dev(t0, t0_value);

    			if (/*e*/ ctx[0].length > 0) {
    				if (if_block) {
    					if_block.p(ctx, dirty);
    				} else {
    					if_block = create_if_block$4(ctx);
    					if_block.c();
    					if_block.m(t2.parentNode, t2);
    				}
    			} else if (if_block) {
    				if_block.d(1);
    				if_block = null;
    			}

    			if (dirty & /*$_*/ 2 && t3_value !== (t3_value = /*$_*/ ctx[1]("logout") + "")) set_data_dev(t3, t3_value);
    		},
    		i: noop,
    		o: noop,
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(h1);
    			if (detaching) detach_dev(t1);
    			if (if_block) if_block.d(detaching);
    			if (detaching) detach_dev(t2);
    			if (detaching) detach_dev(form);
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$8.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function instance$8($$self, $$props, $$invalidate) {
    	let $s;
    	let $_;
    	validate_store(s, 's');
    	component_subscribe($$self, s, $$value => $$invalidate(3, $s = $$value));
    	validate_store($format, '_');
    	component_subscribe($$self, $format, $$value => $$invalidate(1, $_ = $$value));
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Logout', slots, []);
    	let e = "";

    	async function tryLogout() {
    		const res = await APIGet({ method: "logout" });
    		console.log(res);

    		if (res.success) {
    			set_store_value(s, $s.userId = "", $s);
    			set_store_value(s, $s.userName = "", $s);
    			set_store_value(s, $s.loggedIn = false, $s);
    			push("/");
    			return;
    		}

    		$$invalidate(0, e = res.error);
    	}

    	const writable_props = [];

    	Object.keys($$props).forEach(key => {
    		if (!~writable_props.indexOf(key) && key.slice(0, 2) !== '$$' && key !== 'slot') console_1.warn(`<Logout> was created with unknown prop '${key}'`);
    	});

    	$$self.$capture_state = () => ({ _: $format, s, APIGet, push, e, tryLogout, $s, $_ });

    	$$self.$inject_state = $$props => {
    		if ('e' in $$props) $$invalidate(0, e = $$props.e);
    	};

    	if ($$props && "$$inject" in $$props) {
    		$$self.$inject_state($$props.$$inject);
    	}

    	return [e, $_, tryLogout];
    }

    class Logout extends SvelteComponentDev {
    	constructor(options) {
    		super(options);
    		init(this, options, instance$8, create_fragment$8, safe_not_equal, {});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Logout",
    			options,
    			id: create_fragment$8.name
    		});
    	}
    }

    function getEventsAction() {
        const component = get_current_component();
        return node => {
          const events = Object.keys(component.$$.callbacks);
          const listeners = [];

          events.forEach(
              event => listeners.push(
                  listen(node, event, e =>  bubble(component, e))
                )
            );
      
          return {
            destroy: () => {
                listeners.forEach(
                    listener => listener()
                );
            }
          }
        };
    }

    /* node_modules\.pnpm\svelte-chota@1.8.6\node_modules\svelte-chota\cmp\Container.svelte generated by Svelte v3.59.2 */
    const file$7 = "node_modules\\.pnpm\\svelte-chota@1.8.6\\node_modules\\svelte-chota\\cmp\\Container.svelte";

    function create_fragment$7(ctx) {
    	let div;
    	let current;
    	let mounted;
    	let dispose;
    	const default_slot_template = /*#slots*/ ctx[3].default;
    	const default_slot = create_slot(default_slot_template, ctx, /*$$scope*/ ctx[2], null);
    	let div_levels = [/*$$restProps*/ ctx[1]];
    	let div_data = {};

    	for (let i = 0; i < div_levels.length; i += 1) {
    		div_data = assign(div_data, div_levels[i]);
    	}

    	const block = {
    		c: function create() {
    			div = element("div");
    			if (default_slot) default_slot.c();
    			set_attributes(div, div_data);
    			toggle_class(div, "container", 1);
    			add_location(div, file$7, 6, 0, 98);
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, div, anchor);

    			if (default_slot) {
    				default_slot.m(div, null);
    			}

    			current = true;

    			if (!mounted) {
    				dispose = action_destroyer(/*events*/ ctx[0].call(null, div));
    				mounted = true;
    			}
    		},
    		p: function update(ctx, [dirty]) {
    			if (default_slot) {
    				if (default_slot.p && (!current || dirty & /*$$scope*/ 4)) {
    					update_slot_base(
    						default_slot,
    						default_slot_template,
    						ctx,
    						/*$$scope*/ ctx[2],
    						!current
    						? get_all_dirty_from_scope(/*$$scope*/ ctx[2])
    						: get_slot_changes(default_slot_template, /*$$scope*/ ctx[2], dirty, null),
    						null
    					);
    				}
    			}

    			set_attributes(div, div_data = get_spread_update(div_levels, [dirty & /*$$restProps*/ 2 && /*$$restProps*/ ctx[1]]));
    			toggle_class(div, "container", 1);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(default_slot, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(default_slot, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(div);
    			if (default_slot) default_slot.d(detaching);
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$7.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function instance$7($$self, $$props, $$invalidate) {
    	const omit_props_names = [];
    	let $$restProps = compute_rest_props($$props, omit_props_names);
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Container', slots, ['default']);
    	const events = getEventsAction();

    	$$self.$$set = $$new_props => {
    		$$props = assign(assign({}, $$props), exclude_internal_props($$new_props));
    		$$invalidate(1, $$restProps = compute_rest_props($$props, omit_props_names));
    		if ('$$scope' in $$new_props) $$invalidate(2, $$scope = $$new_props.$$scope);
    	};

    	$$self.$capture_state = () => ({ getEventsAction, events });
    	return [events, $$restProps, $$scope, slots];
    }

    class Container extends SvelteComponentDev {
    	constructor(options) {
    		super(options);
    		init(this, options, instance$7, create_fragment$7, safe_not_equal, {});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Container",
    			options,
    			id: create_fragment$7.name
    		});
    	}
    }

    /* node_modules\.pnpm\svelte-chota@1.8.6\node_modules\svelte-chota\cmp\Row.svelte generated by Svelte v3.59.2 */
    const file$6 = "node_modules\\.pnpm\\svelte-chota@1.8.6\\node_modules\\svelte-chota\\cmp\\Row.svelte";

    function create_fragment$6(ctx) {
    	let div;
    	let current;
    	let mounted;
    	let dispose;
    	const default_slot_template = /*#slots*/ ctx[4].default;
    	const default_slot = create_slot(default_slot_template, ctx, /*$$scope*/ ctx[3], null);
    	let div_levels = [/*$$restProps*/ ctx[2]];
    	let div_data = {};

    	for (let i = 0; i < div_levels.length; i += 1) {
    		div_data = assign(div_data, div_levels[i]);
    	}

    	const block = {
    		c: function create() {
    			div = element("div");
    			if (default_slot) default_slot.c();
    			set_attributes(div, div_data);
    			toggle_class(div, "row", 1);
    			toggle_class(div, "reverse", /*reverse*/ ctx[0]);
    			add_location(div, file$6, 8, 0, 137);
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, div, anchor);

    			if (default_slot) {
    				default_slot.m(div, null);
    			}

    			current = true;

    			if (!mounted) {
    				dispose = action_destroyer(/*events*/ ctx[1].call(null, div));
    				mounted = true;
    			}
    		},
    		p: function update(ctx, [dirty]) {
    			if (default_slot) {
    				if (default_slot.p && (!current || dirty & /*$$scope*/ 8)) {
    					update_slot_base(
    						default_slot,
    						default_slot_template,
    						ctx,
    						/*$$scope*/ ctx[3],
    						!current
    						? get_all_dirty_from_scope(/*$$scope*/ ctx[3])
    						: get_slot_changes(default_slot_template, /*$$scope*/ ctx[3], dirty, null),
    						null
    					);
    				}
    			}

    			set_attributes(div, div_data = get_spread_update(div_levels, [dirty & /*$$restProps*/ 4 && /*$$restProps*/ ctx[2]]));
    			toggle_class(div, "row", 1);
    			toggle_class(div, "reverse", /*reverse*/ ctx[0]);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(default_slot, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(default_slot, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(div);
    			if (default_slot) default_slot.d(detaching);
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$6.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function instance$6($$self, $$props, $$invalidate) {
    	const omit_props_names = ["reverse"];
    	let $$restProps = compute_rest_props($$props, omit_props_names);
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Row', slots, ['default']);
    	let { reverse = false } = $$props;
    	const events = getEventsAction();

    	$$self.$$set = $$new_props => {
    		$$props = assign(assign({}, $$props), exclude_internal_props($$new_props));
    		$$invalidate(2, $$restProps = compute_rest_props($$props, omit_props_names));
    		if ('reverse' in $$new_props) $$invalidate(0, reverse = $$new_props.reverse);
    		if ('$$scope' in $$new_props) $$invalidate(3, $$scope = $$new_props.$$scope);
    	};

    	$$self.$capture_state = () => ({ getEventsAction, reverse, events });

    	$$self.$inject_state = $$new_props => {
    		if ('reverse' in $$props) $$invalidate(0, reverse = $$new_props.reverse);
    	};

    	if ($$props && "$$inject" in $$props) {
    		$$self.$inject_state($$props.$$inject);
    	}

    	return [reverse, events, $$restProps, $$scope, slots];
    }

    class Row extends SvelteComponentDev {
    	constructor(options) {
    		super(options);
    		init(this, options, instance$6, create_fragment$6, safe_not_equal, { reverse: 0 });

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Row",
    			options,
    			id: create_fragment$6.name
    		});
    	}

    	get reverse() {
    		throw new Error("<Row>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set reverse(value) {
    		throw new Error("<Row>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}
    }

    /* node_modules\.pnpm\svelte-chota@1.8.6\node_modules\svelte-chota\cmp\Col.svelte generated by Svelte v3.59.2 */
    const file$5 = "node_modules\\.pnpm\\svelte-chota@1.8.6\\node_modules\\svelte-chota\\cmp\\Col.svelte";

    function create_fragment$5(ctx) {
    	let div;
    	let current;
    	let mounted;
    	let dispose;
    	const default_slot_template = /*#slots*/ ctx[7].default;
    	const default_slot = create_slot(default_slot_template, ctx, /*$$scope*/ ctx[6], null);
    	let div_levels = [/*$$restProps*/ ctx[2], { class: /*classes*/ ctx[0] }];
    	let div_data = {};

    	for (let i = 0; i < div_levels.length; i += 1) {
    		div_data = assign(div_data, div_levels[i]);
    	}

    	const block = {
    		c: function create() {
    			div = element("div");
    			if (default_slot) default_slot.c();
    			set_attributes(div, div_data);
    			add_location(div, file$5, 30, 0, 817);
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, div, anchor);

    			if (default_slot) {
    				default_slot.m(div, null);
    			}

    			current = true;

    			if (!mounted) {
    				dispose = action_destroyer(/*events*/ ctx[1].call(null, div));
    				mounted = true;
    			}
    		},
    		p: function update(ctx, [dirty]) {
    			if (default_slot) {
    				if (default_slot.p && (!current || dirty & /*$$scope*/ 64)) {
    					update_slot_base(
    						default_slot,
    						default_slot_template,
    						ctx,
    						/*$$scope*/ ctx[6],
    						!current
    						? get_all_dirty_from_scope(/*$$scope*/ ctx[6])
    						: get_slot_changes(default_slot_template, /*$$scope*/ ctx[6], dirty, null),
    						null
    					);
    				}
    			}

    			set_attributes(div, div_data = get_spread_update(div_levels, [
    				dirty & /*$$restProps*/ 4 && /*$$restProps*/ ctx[2],
    				(!current || dirty & /*classes*/ 1) && { class: /*classes*/ ctx[0] }
    			]));
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(default_slot, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(default_slot, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(div);
    			if (default_slot) default_slot.d(detaching);
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$5.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function instance$5($$self, $$props, $$invalidate) {
    	let classes;
    	const omit_props_names = ["size","sizeMD","sizeLG"];
    	let $$restProps = compute_rest_props($$props, omit_props_names);
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Col', slots, ['default']);
    	let { size = false } = $$props;
    	let { sizeMD = false } = $$props;
    	let { sizeLG = false } = $$props;
    	const events = getEventsAction();

    	function get_col_classes(d, md, lg) {
    		let list = [];
    		if (!size || (size < 1 || size > 12)) list.push('col'); else if (size >= 1 && size <= 12) list.push(`col-${size}`);
    		if (sizeMD) if (sizeMD >= 1 && sizeMD <= 12) list.push(`col-${sizeMD}-md`);
    		if (sizeLG) if (sizeLG >= 1 && sizeLG <= 12) list.push(`col-${sizeLG}-lg`);
    		return list.join(' ');
    	}

    	$$self.$$set = $$new_props => {
    		$$props = assign(assign({}, $$props), exclude_internal_props($$new_props));
    		$$invalidate(2, $$restProps = compute_rest_props($$props, omit_props_names));
    		if ('size' in $$new_props) $$invalidate(3, size = $$new_props.size);
    		if ('sizeMD' in $$new_props) $$invalidate(4, sizeMD = $$new_props.sizeMD);
    		if ('sizeLG' in $$new_props) $$invalidate(5, sizeLG = $$new_props.sizeLG);
    		if ('$$scope' in $$new_props) $$invalidate(6, $$scope = $$new_props.$$scope);
    	};

    	$$self.$capture_state = () => ({
    		getEventsAction,
    		size,
    		sizeMD,
    		sizeLG,
    		events,
    		get_col_classes,
    		classes
    	});

    	$$self.$inject_state = $$new_props => {
    		if ('size' in $$props) $$invalidate(3, size = $$new_props.size);
    		if ('sizeMD' in $$props) $$invalidate(4, sizeMD = $$new_props.sizeMD);
    		if ('sizeLG' in $$props) $$invalidate(5, sizeLG = $$new_props.sizeLG);
    		if ('classes' in $$props) $$invalidate(0, classes = $$new_props.classes);
    	};

    	if ($$props && "$$inject" in $$props) {
    		$$self.$inject_state($$props.$$inject);
    	}

    	$$self.$$.update = () => {
    		$$invalidate(0, classes = $$restProps.hasOwnProperty('class')
    		? $$restProps['class'] + ' ' + get_col_classes()
    		: get_col_classes());
    	};

    	return [classes, events, $$restProps, size, sizeMD, sizeLG, $$scope, slots];
    }

    class Col extends SvelteComponentDev {
    	constructor(options) {
    		super(options);
    		init(this, options, instance$5, create_fragment$5, safe_not_equal, { size: 3, sizeMD: 4, sizeLG: 5 });

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Col",
    			options,
    			id: create_fragment$5.name
    		});
    	}

    	get size() {
    		throw new Error("<Col>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set size(value) {
    		throw new Error("<Col>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get sizeMD() {
    		throw new Error("<Col>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set sizeMD(value) {
    		throw new Error("<Col>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get sizeLG() {
    		throw new Error("<Col>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set sizeLG(value) {
    		throw new Error("<Col>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}
    }

    /* node_modules\.pnpm\svelte-chota@1.8.6\node_modules\svelte-chota\cmp\Card.svelte generated by Svelte v3.59.2 */
    const file$4 = "node_modules\\.pnpm\\svelte-chota@1.8.6\\node_modules\\svelte-chota\\cmp\\Card.svelte";
    const get_footer_slot_changes = dirty => ({});
    const get_footer_slot_context = ctx => ({});
    const get_header_slot_changes = dirty => ({});
    const get_header_slot_context = ctx => ({});

    // (8:0) {#if $$slots.header}
    function create_if_block_1$2(ctx) {
    	let header;
    	let current;
    	const header_slot_template = /*#slots*/ ctx[4].header;
    	const header_slot = create_slot(header_slot_template, ctx, /*$$scope*/ ctx[3], get_header_slot_context);

    	const block = {
    		c: function create() {
    			header = element("header");
    			if (header_slot) header_slot.c();
    			add_location(header, file$4, 8, 1, 169);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, header, anchor);

    			if (header_slot) {
    				header_slot.m(header, null);
    			}

    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			if (header_slot) {
    				if (header_slot.p && (!current || dirty & /*$$scope*/ 8)) {
    					update_slot_base(
    						header_slot,
    						header_slot_template,
    						ctx,
    						/*$$scope*/ ctx[3],
    						!current
    						? get_all_dirty_from_scope(/*$$scope*/ ctx[3])
    						: get_slot_changes(header_slot_template, /*$$scope*/ ctx[3], dirty, get_header_slot_changes),
    						get_header_slot_context
    					);
    				}
    			}
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(header_slot, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(header_slot, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(header);
    			if (header_slot) header_slot.d(detaching);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block_1$2.name,
    		type: "if",
    		source: "(8:0) {#if $$slots.header}",
    		ctx
    	});

    	return block;
    }

    // (14:0) {#if $$slots.footer}
    function create_if_block$3(ctx) {
    	let footer;
    	let current;
    	const footer_slot_template = /*#slots*/ ctx[4].footer;
    	const footer_slot = create_slot(footer_slot_template, ctx, /*$$scope*/ ctx[3], get_footer_slot_context);

    	const block = {
    		c: function create() {
    			footer = element("footer");
    			if (footer_slot) footer_slot.c();
    			add_location(footer, file$4, 14, 1, 267);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, footer, anchor);

    			if (footer_slot) {
    				footer_slot.m(footer, null);
    			}

    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			if (footer_slot) {
    				if (footer_slot.p && (!current || dirty & /*$$scope*/ 8)) {
    					update_slot_base(
    						footer_slot,
    						footer_slot_template,
    						ctx,
    						/*$$scope*/ ctx[3],
    						!current
    						? get_all_dirty_from_scope(/*$$scope*/ ctx[3])
    						: get_slot_changes(footer_slot_template, /*$$scope*/ ctx[3], dirty, get_footer_slot_changes),
    						get_footer_slot_context
    					);
    				}
    			}
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(footer_slot, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(footer_slot, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(footer);
    			if (footer_slot) footer_slot.d(detaching);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block$3.name,
    		type: "if",
    		source: "(14:0) {#if $$slots.footer}",
    		ctx
    	});

    	return block;
    }

    function create_fragment$4(ctx) {
    	let div;
    	let t0;
    	let t1;
    	let current;
    	let mounted;
    	let dispose;
    	let if_block0 = /*$$slots*/ ctx[2].header && create_if_block_1$2(ctx);
    	const default_slot_template = /*#slots*/ ctx[4].default;
    	const default_slot = create_slot(default_slot_template, ctx, /*$$scope*/ ctx[3], null);
    	let if_block1 = /*$$slots*/ ctx[2].footer && create_if_block$3(ctx);
    	let div_levels = [/*$$restProps*/ ctx[1]];
    	let div_data = {};

    	for (let i = 0; i < div_levels.length; i += 1) {
    		div_data = assign(div_data, div_levels[i]);
    	}

    	const block = {
    		c: function create() {
    			div = element("div");
    			if (if_block0) if_block0.c();
    			t0 = space();
    			if (default_slot) default_slot.c();
    			t1 = space();
    			if (if_block1) if_block1.c();
    			set_attributes(div, div_data);
    			toggle_class(div, "card", 1);
    			add_location(div, file$4, 6, 0, 98);
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, div, anchor);
    			if (if_block0) if_block0.m(div, null);
    			append_dev(div, t0);

    			if (default_slot) {
    				default_slot.m(div, null);
    			}

    			append_dev(div, t1);
    			if (if_block1) if_block1.m(div, null);
    			current = true;

    			if (!mounted) {
    				dispose = action_destroyer(/*events*/ ctx[0].call(null, div));
    				mounted = true;
    			}
    		},
    		p: function update(ctx, [dirty]) {
    			if (/*$$slots*/ ctx[2].header) {
    				if (if_block0) {
    					if_block0.p(ctx, dirty);

    					if (dirty & /*$$slots*/ 4) {
    						transition_in(if_block0, 1);
    					}
    				} else {
    					if_block0 = create_if_block_1$2(ctx);
    					if_block0.c();
    					transition_in(if_block0, 1);
    					if_block0.m(div, t0);
    				}
    			} else if (if_block0) {
    				group_outros();

    				transition_out(if_block0, 1, 1, () => {
    					if_block0 = null;
    				});

    				check_outros();
    			}

    			if (default_slot) {
    				if (default_slot.p && (!current || dirty & /*$$scope*/ 8)) {
    					update_slot_base(
    						default_slot,
    						default_slot_template,
    						ctx,
    						/*$$scope*/ ctx[3],
    						!current
    						? get_all_dirty_from_scope(/*$$scope*/ ctx[3])
    						: get_slot_changes(default_slot_template, /*$$scope*/ ctx[3], dirty, null),
    						null
    					);
    				}
    			}

    			if (/*$$slots*/ ctx[2].footer) {
    				if (if_block1) {
    					if_block1.p(ctx, dirty);

    					if (dirty & /*$$slots*/ 4) {
    						transition_in(if_block1, 1);
    					}
    				} else {
    					if_block1 = create_if_block$3(ctx);
    					if_block1.c();
    					transition_in(if_block1, 1);
    					if_block1.m(div, null);
    				}
    			} else if (if_block1) {
    				group_outros();

    				transition_out(if_block1, 1, 1, () => {
    					if_block1 = null;
    				});

    				check_outros();
    			}

    			set_attributes(div, div_data = get_spread_update(div_levels, [dirty & /*$$restProps*/ 2 && /*$$restProps*/ ctx[1]]));
    			toggle_class(div, "card", 1);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(if_block0);
    			transition_in(default_slot, local);
    			transition_in(if_block1);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(if_block0);
    			transition_out(default_slot, local);
    			transition_out(if_block1);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(div);
    			if (if_block0) if_block0.d();
    			if (default_slot) default_slot.d(detaching);
    			if (if_block1) if_block1.d();
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$4.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function instance$4($$self, $$props, $$invalidate) {
    	const omit_props_names = [];
    	let $$restProps = compute_rest_props($$props, omit_props_names);
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Card', slots, ['header','default','footer']);
    	const $$slots = compute_slots(slots);
    	const events = getEventsAction();

    	$$self.$$set = $$new_props => {
    		$$props = assign(assign({}, $$props), exclude_internal_props($$new_props));
    		$$invalidate(1, $$restProps = compute_rest_props($$props, omit_props_names));
    		if ('$$scope' in $$new_props) $$invalidate(3, $$scope = $$new_props.$$scope);
    	};

    	$$self.$capture_state = () => ({ getEventsAction, events });
    	return [events, $$restProps, $$slots, $$scope, slots];
    }

    class Card extends SvelteComponentDev {
    	constructor(options) {
    		super(options);
    		init(this, options, instance$4, create_fragment$4, safe_not_equal, {});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Card",
    			options,
    			id: create_fragment$4.name
    		});
    	}
    }

    /* node_modules\.pnpm\svelte-chota@1.8.6\node_modules\svelte-chota\cmp\Icon.svelte generated by Svelte v3.59.2 */
    const file$3 = "node_modules\\.pnpm\\svelte-chota@1.8.6\\node_modules\\svelte-chota\\cmp\\Icon.svelte";

    // (79:0) {:else}
    function create_else_block$2(ctx) {
    	let svg;
    	let mounted;
    	let dispose;

    	function select_block_type_1(ctx, dirty) {
    		if (/*spin*/ ctx[0] !== false) return create_if_block_2$1;
    		return create_else_block_1;
    	}

    	let current_block_type = select_block_type_1(ctx);
    	let if_block = current_block_type(ctx);
    	let svg_levels = [{ viewBox: "0 0 24 24" }, { style: /*style*/ ctx[5] }, /*$$restProps*/ ctx[9]];
    	let svg_data = {};

    	for (let i = 0; i < svg_levels.length; i += 1) {
    		svg_data = assign(svg_data, svg_levels[i]);
    	}

    	const block = {
    		c: function create() {
    			svg = svg_element("svg");
    			if_block.c();
    			set_svg_attributes(svg, svg_data);
    			toggle_class(svg, "svelte-1q4wean", true);
    			add_location(svg, file$3, 79, 1, 2046);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, svg, anchor);
    			if_block.m(svg, null);

    			if (!mounted) {
    				dispose = action_destroyer(/*events*/ ctx[8].call(null, svg));
    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if (current_block_type === (current_block_type = select_block_type_1(ctx)) && if_block) {
    				if_block.p(ctx, dirty);
    			} else {
    				if_block.d(1);
    				if_block = current_block_type(ctx);

    				if (if_block) {
    					if_block.c();
    					if_block.m(svg, null);
    				}
    			}

    			set_svg_attributes(svg, svg_data = get_spread_update(svg_levels, [
    				{ viewBox: "0 0 24 24" },
    				dirty & /*style*/ 32 && { style: /*style*/ ctx[5] },
    				dirty & /*$$restProps*/ 512 && /*$$restProps*/ ctx[9]
    			]));

    			toggle_class(svg, "svelte-1q4wean", true);
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(svg);
    			if_block.d();
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_else_block$2.name,
    		type: "else",
    		source: "(79:0) {:else}",
    		ctx
    	});

    	return block;
    }

    // (75:14) 
    function create_if_block_1$1(ctx) {
    	let svg;
    	let use_1;
    	let mounted;
    	let dispose;
    	let svg_levels = [{ viewBox: "0 0 24 24" }, { style: /*style*/ ctx[5] }, /*$$restProps*/ ctx[9]];
    	let svg_data = {};

    	for (let i = 0; i < svg_levels.length; i += 1) {
    		svg_data = assign(svg_data, svg_levels[i]);
    	}

    	const block = {
    		c: function create() {
    			svg = svg_element("svg");
    			use_1 = svg_element("use");
    			xlink_attr(use_1, "xlink:href", /*use*/ ctx[2]);
    			attr_dev(use_1, "style", /*aniStyle*/ ctx[4]);
    			attr_dev(use_1, "class", "svelte-1q4wean");
    			toggle_class(use_1, "spinCW", /*spinCW*/ ctx[7]);
    			toggle_class(use_1, "spinCCW", /*spinCCW*/ ctx[6]);
    			add_location(use_1, file$3, 76, 2, 1956);
    			set_svg_attributes(svg, svg_data);
    			toggle_class(svg, "svelte-1q4wean", true);
    			add_location(svg, file$3, 75, 1, 1892);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, svg, anchor);
    			append_dev(svg, use_1);

    			if (!mounted) {
    				dispose = action_destroyer(/*events*/ ctx[8].call(null, svg));
    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*use*/ 4) {
    				xlink_attr(use_1, "xlink:href", /*use*/ ctx[2]);
    			}

    			if (dirty & /*aniStyle*/ 16) {
    				attr_dev(use_1, "style", /*aniStyle*/ ctx[4]);
    			}

    			if (dirty & /*spinCW*/ 128) {
    				toggle_class(use_1, "spinCW", /*spinCW*/ ctx[7]);
    			}

    			if (dirty & /*spinCCW*/ 64) {
    				toggle_class(use_1, "spinCCW", /*spinCCW*/ ctx[6]);
    			}

    			set_svg_attributes(svg, svg_data = get_spread_update(svg_levels, [
    				{ viewBox: "0 0 24 24" },
    				dirty & /*style*/ 32 && { style: /*style*/ ctx[5] },
    				dirty & /*$$restProps*/ 512 && /*$$restProps*/ ctx[9]
    			]));

    			toggle_class(svg, "svelte-1q4wean", true);
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(svg);
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block_1$1.name,
    		type: "if",
    		source: "(75:14) ",
    		ctx
    	});

    	return block;
    }

    // (71:0) {#if url}
    function create_if_block$2(ctx) {
    	let span;
    	let img;
    	let img_src_value;
    	let mounted;
    	let dispose;
    	let span_levels = [{ style: /*style*/ ctx[5] }, /*$$restProps*/ ctx[9]];
    	let span_data = {};

    	for (let i = 0; i < span_levels.length; i += 1) {
    		span_data = assign(span_data, span_levels[i]);
    	}

    	const block = {
    		c: function create() {
    			span = element("span");
    			img = element("img");
    			if (!src_url_equal(img.src, img_src_value = /*url*/ ctx[3])) attr_dev(img, "src", img_src_value);
    			attr_dev(img, "alt", "");
    			attr_dev(img, "width", "100%");
    			attr_dev(img, "height", "100%");
    			attr_dev(img, "style", /*aniStyle*/ ctx[4]);
    			attr_dev(img, "class", "svelte-1q4wean");
    			toggle_class(img, "spinCW", /*spinCW*/ ctx[7]);
    			toggle_class(img, "spinCCW", /*spinCCW*/ ctx[6]);
    			add_location(img, file$3, 72, 2, 1769);
    			set_attributes(span, span_data);
    			toggle_class(span, "svelte-1q4wean", true);
    			add_location(span, file$3, 71, 1, 1724);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, span, anchor);
    			append_dev(span, img);

    			if (!mounted) {
    				dispose = action_destroyer(/*events*/ ctx[8].call(null, span));
    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*url*/ 8 && !src_url_equal(img.src, img_src_value = /*url*/ ctx[3])) {
    				attr_dev(img, "src", img_src_value);
    			}

    			if (dirty & /*aniStyle*/ 16) {
    				attr_dev(img, "style", /*aniStyle*/ ctx[4]);
    			}

    			if (dirty & /*spinCW*/ 128) {
    				toggle_class(img, "spinCW", /*spinCW*/ ctx[7]);
    			}

    			if (dirty & /*spinCCW*/ 64) {
    				toggle_class(img, "spinCCW", /*spinCCW*/ ctx[6]);
    			}

    			set_attributes(span, span_data = get_spread_update(span_levels, [
    				dirty & /*style*/ 32 && { style: /*style*/ ctx[5] },
    				dirty & /*$$restProps*/ 512 && /*$$restProps*/ ctx[9]
    			]));

    			toggle_class(span, "svelte-1q4wean", true);
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(span);
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block$2.name,
    		type: "if",
    		source: "(71:0) {#if url}",
    		ctx
    	});

    	return block;
    }

    // (85:1) {:else}
    function create_else_block_1(ctx) {
    	let path_1;

    	const block = {
    		c: function create() {
    			path_1 = svg_element("path");
    			attr_dev(path_1, "d", /*path*/ ctx[1]);
    			add_location(path_1, file$3, 85, 2, 2224);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, path_1, anchor);
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*path*/ 2) {
    				attr_dev(path_1, "d", /*path*/ ctx[1]);
    			}
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(path_1);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_else_block_1.name,
    		type: "else",
    		source: "(85:1) {:else}",
    		ctx
    	});

    	return block;
    }

    // (81:1) {#if spin !== false}
    function create_if_block_2$1(ctx) {
    	let g;
    	let path_1;

    	const block = {
    		c: function create() {
    			g = svg_element("g");
    			path_1 = svg_element("path");
    			attr_dev(path_1, "d", /*path*/ ctx[1]);
    			add_location(path_1, file$3, 82, 3, 2183);
    			attr_dev(g, "style", /*aniStyle*/ ctx[4]);
    			attr_dev(g, "class", "svelte-1q4wean");
    			toggle_class(g, "spinCW", /*spinCW*/ ctx[7]);
    			toggle_class(g, "spinCCW", /*spinCCW*/ ctx[6]);
    			add_location(g, file$3, 81, 2, 2132);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, g, anchor);
    			append_dev(g, path_1);
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*path*/ 2) {
    				attr_dev(path_1, "d", /*path*/ ctx[1]);
    			}

    			if (dirty & /*aniStyle*/ 16) {
    				attr_dev(g, "style", /*aniStyle*/ ctx[4]);
    			}

    			if (dirty & /*spinCW*/ 128) {
    				toggle_class(g, "spinCW", /*spinCW*/ ctx[7]);
    			}

    			if (dirty & /*spinCCW*/ 64) {
    				toggle_class(g, "spinCCW", /*spinCCW*/ ctx[6]);
    			}
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(g);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block_2$1.name,
    		type: "if",
    		source: "(81:1) {#if spin !== false}",
    		ctx
    	});

    	return block;
    }

    function create_fragment$3(ctx) {
    	let if_block_anchor;

    	function select_block_type(ctx, dirty) {
    		if (/*url*/ ctx[3]) return create_if_block$2;
    		if (/*use*/ ctx[2]) return create_if_block_1$1;
    		return create_else_block$2;
    	}

    	let current_block_type = select_block_type(ctx);
    	let if_block = current_block_type(ctx);

    	const block = {
    		c: function create() {
    			if_block.c();
    			if_block_anchor = empty();
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			if_block.m(target, anchor);
    			insert_dev(target, if_block_anchor, anchor);
    		},
    		p: function update(ctx, [dirty]) {
    			if (current_block_type === (current_block_type = select_block_type(ctx)) && if_block) {
    				if_block.p(ctx, dirty);
    			} else {
    				if_block.d(1);
    				if_block = current_block_type(ctx);

    				if (if_block) {
    					if_block.c();
    					if_block.m(if_block_anchor.parentNode, if_block_anchor);
    				}
    			}
    		},
    		i: noop,
    		o: noop,
    		d: function destroy(detaching) {
    			if_block.d(detaching);
    			if (detaching) detach_dev(if_block_anchor);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$3.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function instance$3($$self, $$props, $$invalidate) {
    	let inverse;
    	let spintime;
    	let spinCW;
    	let spinCCW;
    	let style;
    	let aniStyle;
    	const omit_props_names = ["src","size","color","flipH","flipV","rotate","spin"];
    	let $$restProps = compute_rest_props($$props, omit_props_names);
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Icon', slots, []);
    	const events = getEventsAction();
    	let { src = null } = $$props;
    	let { size = 1 } = $$props;
    	let { color = null } = $$props;
    	let { flipH = null } = $$props;
    	let { flipV = null } = $$props;
    	let { rotate = 0 } = $$props;
    	let { spin = false } = $$props;
    	let path = false;
    	let use = false;
    	let url = false;

    	// size
    	if (Number(size)) size = Number(size);

    	// styles
    	const getStyles = () => {
    		const transform = [];
    		const styles = [];

    		if (size !== null) {
    			const width = typeof size === "string" ? size : `${size * 1.5}rem`;
    			styles.push(['width', width]);
    			styles.push(['height', width]);
    		}

    		styles.push(['fill', color !== null ? color : 'currentColor']);

    		if (flipH) {
    			transform.push("scaleX(-1)");
    		}

    		if (flipV) {
    			transform.push("scaleY(-1)");
    		}

    		if (rotate != 0) {
    			transform.push(`rotate(${rotate}deg)`);
    		}

    		if (transform.length > 0) {
    			styles.push(['transform', transform.join(' ')]);
    			styles.push(['transform-origin', 'center']);
    		}

    		return styles.reduce(
    			(cur, item) => {
    				return `${cur} ${item[0]}:${item[1]};`;
    			},
    			''
    		);
    	};

    	$$self.$$set = $$new_props => {
    		$$props = assign(assign({}, $$props), exclude_internal_props($$new_props));
    		$$invalidate(9, $$restProps = compute_rest_props($$props, omit_props_names));
    		if ('src' in $$new_props) $$invalidate(11, src = $$new_props.src);
    		if ('size' in $$new_props) $$invalidate(10, size = $$new_props.size);
    		if ('color' in $$new_props) $$invalidate(12, color = $$new_props.color);
    		if ('flipH' in $$new_props) $$invalidate(13, flipH = $$new_props.flipH);
    		if ('flipV' in $$new_props) $$invalidate(14, flipV = $$new_props.flipV);
    		if ('rotate' in $$new_props) $$invalidate(15, rotate = $$new_props.rotate);
    		if ('spin' in $$new_props) $$invalidate(0, spin = $$new_props.spin);
    	};

    	$$self.$capture_state = () => ({
    		getEventsAction,
    		events,
    		src,
    		size,
    		color,
    		flipH,
    		flipV,
    		rotate,
    		spin,
    		path,
    		use,
    		url,
    		getStyles,
    		spintime,
    		aniStyle,
    		style,
    		inverse,
    		spinCCW,
    		spinCW
    	});

    	$$self.$inject_state = $$new_props => {
    		if ('src' in $$props) $$invalidate(11, src = $$new_props.src);
    		if ('size' in $$props) $$invalidate(10, size = $$new_props.size);
    		if ('color' in $$props) $$invalidate(12, color = $$new_props.color);
    		if ('flipH' in $$props) $$invalidate(13, flipH = $$new_props.flipH);
    		if ('flipV' in $$props) $$invalidate(14, flipV = $$new_props.flipV);
    		if ('rotate' in $$props) $$invalidate(15, rotate = $$new_props.rotate);
    		if ('spin' in $$props) $$invalidate(0, spin = $$new_props.spin);
    		if ('path' in $$props) $$invalidate(1, path = $$new_props.path);
    		if ('use' in $$props) $$invalidate(2, use = $$new_props.use);
    		if ('url' in $$props) $$invalidate(3, url = $$new_props.url);
    		if ('spintime' in $$props) $$invalidate(16, spintime = $$new_props.spintime);
    		if ('aniStyle' in $$props) $$invalidate(4, aniStyle = $$new_props.aniStyle);
    		if ('style' in $$props) $$invalidate(5, style = $$new_props.style);
    		if ('inverse' in $$props) $$invalidate(17, inverse = $$new_props.inverse);
    		if ('spinCCW' in $$props) $$invalidate(6, spinCCW = $$new_props.spinCCW);
    		if ('spinCW' in $$props) $$invalidate(7, spinCW = $$new_props.spinCW);
    	};

    	if ($$props && "$$inject" in $$props) {
    		$$self.$inject_state($$props.$$inject);
    	}

    	$$self.$$.update = () => {
    		if ($$self.$$.dirty & /*src*/ 2048) {
    			//Icon source
    			if (!!src && src.toLowerCase().trim().endsWith('.svg')) {
    				$$invalidate(3, url = src);
    				$$invalidate(1, path = $$invalidate(2, use = false));
    			} else if (!!src && src.toLowerCase().trim().includes('.svg#')) {
    				$$invalidate(2, use = src);
    				$$invalidate(3, url = $$invalidate(1, path = false));
    			} else if (!!src) {
    				$$invalidate(1, path = src);
    				$$invalidate(3, url = $$invalidate(2, use = false));
    			}
    		}

    		if ($$self.$$.dirty & /*spin*/ 1) {
    			// SPIN properties
    			$$invalidate(17, inverse = typeof spin !== "boolean" && spin < 0 ? true : false);
    		}

    		if ($$self.$$.dirty & /*spin*/ 1) {
    			$$invalidate(16, spintime = Math.abs(spin === true ? 2 : spin));
    		}

    		if ($$self.$$.dirty & /*spin, inverse*/ 131073) {
    			$$invalidate(7, spinCW = !!spin && !inverse);
    		}

    		if ($$self.$$.dirty & /*spin, inverse*/ 131073) {
    			$$invalidate(6, spinCCW = !!spin && inverse);
    		}

    		if ($$self.$$.dirty & /*size, color, flipH, flipV, rotate*/ 62464) {
    			$$invalidate(5, style = getStyles());
    		}

    		if ($$self.$$.dirty & /*spin, spintime*/ 65537) {
    			$$invalidate(4, aniStyle = !!spin ? `animation-duration: ${spintime}s` : undefined);
    		}
    	};

    	return [
    		spin,
    		path,
    		use,
    		url,
    		aniStyle,
    		style,
    		spinCCW,
    		spinCW,
    		events,
    		$$restProps,
    		size,
    		src,
    		color,
    		flipH,
    		flipV,
    		rotate,
    		spintime,
    		inverse
    	];
    }

    class Icon extends SvelteComponentDev {
    	constructor(options) {
    		super(options);

    		init(this, options, instance$3, create_fragment$3, safe_not_equal, {
    			src: 11,
    			size: 10,
    			color: 12,
    			flipH: 13,
    			flipV: 14,
    			rotate: 15,
    			spin: 0
    		});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Icon",
    			options,
    			id: create_fragment$3.name
    		});
    	}

    	get src() {
    		throw new Error("<Icon>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set src(value) {
    		throw new Error("<Icon>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get size() {
    		throw new Error("<Icon>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set size(value) {
    		throw new Error("<Icon>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get color() {
    		throw new Error("<Icon>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set color(value) {
    		throw new Error("<Icon>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get flipH() {
    		throw new Error("<Icon>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set flipH(value) {
    		throw new Error("<Icon>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get flipV() {
    		throw new Error("<Icon>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set flipV(value) {
    		throw new Error("<Icon>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get rotate() {
    		throw new Error("<Icon>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set rotate(value) {
    		throw new Error("<Icon>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get spin() {
    		throw new Error("<Icon>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set spin(value) {
    		throw new Error("<Icon>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}
    }

    /* node_modules\.pnpm\svelte-chota@1.8.6\node_modules\svelte-chota\cmp\Button.svelte generated by Svelte v3.59.2 */
    const file$2 = "node_modules\\.pnpm\\svelte-chota@1.8.6\\node_modules\\svelte-chota\\cmp\\Button.svelte";

    // (87:0) {:else}
    function create_else_block$1(ctx) {
    	let details;
    	let summary;
    	let t0;

    	let t1_value = (/*dropdown*/ ctx[11] !== true
    	? /*dropdown*/ ctx[11]
    	: '') + "";

    	let t1;
    	let t2;
    	let t3;
    	let card;
    	let dropdownAction_action;
    	let current;
    	let mounted;
    	let dispose;
    	let if_block0 = /*icon*/ ctx[9] && create_if_block_4(ctx);
    	let if_block1 = /*iconRight*/ ctx[10] && create_if_block_3(ctx);
    	let summary_levels = [/*$$restProps*/ ctx[17]];
    	let summary_data = {};

    	for (let i = 0; i < summary_levels.length; i += 1) {
    		summary_data = assign(summary_data, summary_levels[i]);
    	}

    	card = new Card({
    			props: {
    				style: "z-index:1",
    				$$slots: { default: [create_default_slot$2] },
    				$$scope: { ctx }
    			},
    			$$inline: true
    		});

    	const block = {
    		c: function create() {
    			details = element("details");
    			summary = element("summary");
    			if (if_block0) if_block0.c();
    			t0 = space();
    			t1 = text(t1_value);
    			t2 = space();
    			if (if_block1) if_block1.c();
    			t3 = space();
    			create_component(card.$$.fragment);
    			set_attributes(summary, summary_data);
    			toggle_class(summary, "button", 1);
    			toggle_class(summary, "outline", /*outline*/ ctx[1]);
    			toggle_class(summary, "primary", /*primary*/ ctx[2]);
    			toggle_class(summary, "secondary", /*secondary*/ ctx[3]);
    			toggle_class(summary, "dark", /*dark*/ ctx[4]);
    			toggle_class(summary, "error", /*error*/ ctx[5]);
    			toggle_class(summary, "success", /*success*/ ctx[6]);
    			toggle_class(summary, "clear", /*clear*/ ctx[7]);
    			toggle_class(summary, "loading", /*loading*/ ctx[8]);
    			toggle_class(summary, "icon", /*clIcon*/ ctx[15]);
    			toggle_class(summary, "icon-only", /*clIcononly*/ ctx[14]);
    			toggle_class(summary, "svelte-1o5ccdl", true);
    			add_location(summary, file$2, 88, 4, 2300);
    			attr_dev(details, "class", "dropdown");
    			add_location(details, file$2, 87, 2, 2228);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, details, anchor);
    			append_dev(details, summary);
    			if (if_block0) if_block0.m(summary, null);
    			append_dev(summary, t0);
    			append_dev(summary, t1);
    			append_dev(summary, t2);
    			if (if_block1) if_block1.m(summary, null);
    			append_dev(details, t3);
    			mount_component(card, details, null);
    			details.open = /*open*/ ctx[0];
    			current = true;

    			if (!mounted) {
    				dispose = [
    					action_destroyer(/*events*/ ctx[16].call(null, summary)),
    					listen_dev(details, "toggle", /*details_toggle_handler*/ ctx[19]),
    					action_destroyer(dropdownAction_action = dropdownAction.call(null, details, /*autoclose*/ ctx[12]))
    				];

    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if (/*icon*/ ctx[9]) {
    				if (if_block0) {
    					if_block0.p(ctx, dirty);

    					if (dirty & /*icon*/ 512) {
    						transition_in(if_block0, 1);
    					}
    				} else {
    					if_block0 = create_if_block_4(ctx);
    					if_block0.c();
    					transition_in(if_block0, 1);
    					if_block0.m(summary, t0);
    				}
    			} else if (if_block0) {
    				group_outros();

    				transition_out(if_block0, 1, 1, () => {
    					if_block0 = null;
    				});

    				check_outros();
    			}

    			if ((!current || dirty & /*dropdown*/ 2048) && t1_value !== (t1_value = (/*dropdown*/ ctx[11] !== true
    			? /*dropdown*/ ctx[11]
    			: '') + "")) set_data_maybe_contenteditable_dev(t1, t1_value, summary_data['contenteditable']);

    			if (/*iconRight*/ ctx[10]) {
    				if (if_block1) {
    					if_block1.p(ctx, dirty);

    					if (dirty & /*iconRight*/ 1024) {
    						transition_in(if_block1, 1);
    					}
    				} else {
    					if_block1 = create_if_block_3(ctx);
    					if_block1.c();
    					transition_in(if_block1, 1);
    					if_block1.m(summary, null);
    				}
    			} else if (if_block1) {
    				group_outros();

    				transition_out(if_block1, 1, 1, () => {
    					if_block1 = null;
    				});

    				check_outros();
    			}

    			set_attributes(summary, summary_data = get_spread_update(summary_levels, [dirty & /*$$restProps*/ 131072 && /*$$restProps*/ ctx[17]]));
    			toggle_class(summary, "button", 1);
    			toggle_class(summary, "outline", /*outline*/ ctx[1]);
    			toggle_class(summary, "primary", /*primary*/ ctx[2]);
    			toggle_class(summary, "secondary", /*secondary*/ ctx[3]);
    			toggle_class(summary, "dark", /*dark*/ ctx[4]);
    			toggle_class(summary, "error", /*error*/ ctx[5]);
    			toggle_class(summary, "success", /*success*/ ctx[6]);
    			toggle_class(summary, "clear", /*clear*/ ctx[7]);
    			toggle_class(summary, "loading", /*loading*/ ctx[8]);
    			toggle_class(summary, "icon", /*clIcon*/ ctx[15]);
    			toggle_class(summary, "icon-only", /*clIcononly*/ ctx[14]);
    			toggle_class(summary, "svelte-1o5ccdl", true);
    			const card_changes = {};

    			if (dirty & /*$$scope*/ 1048576) {
    				card_changes.$$scope = { dirty, ctx };
    			}

    			card.$set(card_changes);

    			if (dirty & /*open*/ 1) {
    				details.open = /*open*/ ctx[0];
    			}

    			if (dropdownAction_action && is_function(dropdownAction_action.update) && dirty & /*autoclose*/ 4096) dropdownAction_action.update.call(null, /*autoclose*/ ctx[12]);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(if_block0);
    			transition_in(if_block1);
    			transition_in(card.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(if_block0);
    			transition_out(if_block1);
    			transition_out(card.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(details);
    			if (if_block0) if_block0.d();
    			if (if_block1) if_block1.d();
    			destroy_component(card);
    			mounted = false;
    			run_all(dispose);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_else_block$1.name,
    		type: "else",
    		source: "(87:0) {:else}",
    		ctx
    	});

    	return block;
    }

    // (64:0) {#if dropdown === false}
    function create_if_block$1(ctx) {
    	let button;
    	let t0;
    	let t1;
    	let button_type_value;
    	let current;
    	let mounted;
    	let dispose;
    	let if_block0 = /*icon*/ ctx[9] && create_if_block_2(ctx);
    	const default_slot_template = /*#slots*/ ctx[18].default;
    	const default_slot = create_slot(default_slot_template, ctx, /*$$scope*/ ctx[20], null);
    	let if_block1 = /*iconRight*/ ctx[10] && create_if_block_1(ctx);

    	let button_levels = [
    		/*$$restProps*/ ctx[17],
    		{
    			type: button_type_value = /*submit*/ ctx[13] ? 'submit' : 'button'
    		}
    	];

    	let button_data = {};

    	for (let i = 0; i < button_levels.length; i += 1) {
    		button_data = assign(button_data, button_levels[i]);
    	}

    	const block = {
    		c: function create() {
    			button = element("button");
    			if (if_block0) if_block0.c();
    			t0 = space();
    			if (default_slot) default_slot.c();
    			t1 = space();
    			if (if_block1) if_block1.c();
    			set_attributes(button, button_data);
    			toggle_class(button, "button", 1);
    			toggle_class(button, "outline", /*outline*/ ctx[1]);
    			toggle_class(button, "primary", /*primary*/ ctx[2]);
    			toggle_class(button, "secondary", /*secondary*/ ctx[3]);
    			toggle_class(button, "dark", /*dark*/ ctx[4]);
    			toggle_class(button, "error", /*error*/ ctx[5]);
    			toggle_class(button, "success", /*success*/ ctx[6]);
    			toggle_class(button, "clear", /*clear*/ ctx[7]);
    			toggle_class(button, "loading", /*loading*/ ctx[8]);
    			toggle_class(button, "icon", /*clIcon*/ ctx[15]);
    			toggle_class(button, "icon-only", /*clIcononly*/ ctx[14]);
    			toggle_class(button, "svelte-1o5ccdl", true);
    			add_location(button, file$2, 64, 0, 1718);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, button, anchor);
    			if (if_block0) if_block0.m(button, null);
    			append_dev(button, t0);

    			if (default_slot) {
    				default_slot.m(button, null);
    			}

    			append_dev(button, t1);
    			if (if_block1) if_block1.m(button, null);
    			if (button.autofocus) button.focus();
    			current = true;

    			if (!mounted) {
    				dispose = action_destroyer(/*events*/ ctx[16].call(null, button));
    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if (/*icon*/ ctx[9]) {
    				if (if_block0) {
    					if_block0.p(ctx, dirty);

    					if (dirty & /*icon*/ 512) {
    						transition_in(if_block0, 1);
    					}
    				} else {
    					if_block0 = create_if_block_2(ctx);
    					if_block0.c();
    					transition_in(if_block0, 1);
    					if_block0.m(button, t0);
    				}
    			} else if (if_block0) {
    				group_outros();

    				transition_out(if_block0, 1, 1, () => {
    					if_block0 = null;
    				});

    				check_outros();
    			}

    			if (default_slot) {
    				if (default_slot.p && (!current || dirty & /*$$scope*/ 1048576)) {
    					update_slot_base(
    						default_slot,
    						default_slot_template,
    						ctx,
    						/*$$scope*/ ctx[20],
    						!current
    						? get_all_dirty_from_scope(/*$$scope*/ ctx[20])
    						: get_slot_changes(default_slot_template, /*$$scope*/ ctx[20], dirty, null),
    						null
    					);
    				}
    			}

    			if (/*iconRight*/ ctx[10]) {
    				if (if_block1) {
    					if_block1.p(ctx, dirty);

    					if (dirty & /*iconRight*/ 1024) {
    						transition_in(if_block1, 1);
    					}
    				} else {
    					if_block1 = create_if_block_1(ctx);
    					if_block1.c();
    					transition_in(if_block1, 1);
    					if_block1.m(button, null);
    				}
    			} else if (if_block1) {
    				group_outros();

    				transition_out(if_block1, 1, 1, () => {
    					if_block1 = null;
    				});

    				check_outros();
    			}

    			set_attributes(button, button_data = get_spread_update(button_levels, [
    				dirty & /*$$restProps*/ 131072 && /*$$restProps*/ ctx[17],
    				(!current || dirty & /*submit*/ 8192 && button_type_value !== (button_type_value = /*submit*/ ctx[13] ? 'submit' : 'button')) && { type: button_type_value }
    			]));

    			toggle_class(button, "button", 1);
    			toggle_class(button, "outline", /*outline*/ ctx[1]);
    			toggle_class(button, "primary", /*primary*/ ctx[2]);
    			toggle_class(button, "secondary", /*secondary*/ ctx[3]);
    			toggle_class(button, "dark", /*dark*/ ctx[4]);
    			toggle_class(button, "error", /*error*/ ctx[5]);
    			toggle_class(button, "success", /*success*/ ctx[6]);
    			toggle_class(button, "clear", /*clear*/ ctx[7]);
    			toggle_class(button, "loading", /*loading*/ ctx[8]);
    			toggle_class(button, "icon", /*clIcon*/ ctx[15]);
    			toggle_class(button, "icon-only", /*clIcononly*/ ctx[14]);
    			toggle_class(button, "svelte-1o5ccdl", true);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(if_block0);
    			transition_in(default_slot, local);
    			transition_in(if_block1);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(if_block0);
    			transition_out(default_slot, local);
    			transition_out(if_block1);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(button);
    			if (if_block0) if_block0.d();
    			if (default_slot) default_slot.d(detaching);
    			if (if_block1) if_block1.d();
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block$1.name,
    		type: "if",
    		source: "(64:0) {#if dropdown === false}",
    		ctx
    	});

    	return block;
    }

    // (106:4) {#if icon}
    function create_if_block_4(ctx) {
    	let span;
    	let icon_1;
    	let current;

    	icon_1 = new Icon({
    			props: { src: /*icon*/ ctx[9], size: "24px" },
    			$$inline: true
    		});

    	const block = {
    		c: function create() {
    			span = element("span");
    			create_component(icon_1.$$.fragment);
    			attr_dev(span, "class", "lefticon svelte-1o5ccdl");
    			add_location(span, file$2, 105, 15, 2645);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, span, anchor);
    			mount_component(icon_1, span, null);
    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			const icon_1_changes = {};
    			if (dirty & /*icon*/ 512) icon_1_changes.src = /*icon*/ ctx[9];
    			icon_1.$set(icon_1_changes);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(icon_1.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(icon_1.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(span);
    			destroy_component(icon_1);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block_4.name,
    		type: "if",
    		source: "(106:4) {#if icon}",
    		ctx
    	});

    	return block;
    }

    // (108:4) {#if iconRight}
    function create_if_block_3(ctx) {
    	let span;
    	let icon_1;
    	let current;

    	icon_1 = new Icon({
    			props: { src: /*iconRight*/ ctx[10], size: "24px" },
    			$$inline: true
    		});

    	const block = {
    		c: function create() {
    			span = element("span");
    			create_component(icon_1.$$.fragment);
    			attr_dev(span, "class", "righticon svelte-1o5ccdl");
    			add_location(span, file$2, 107, 20, 2777);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, span, anchor);
    			mount_component(icon_1, span, null);
    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			const icon_1_changes = {};
    			if (dirty & /*iconRight*/ 1024) icon_1_changes.src = /*iconRight*/ ctx[10];
    			icon_1.$set(icon_1_changes);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(icon_1.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(icon_1.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(span);
    			destroy_component(icon_1);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block_3.name,
    		type: "if",
    		source: "(108:4) {#if iconRight}",
    		ctx
    	});

    	return block;
    }

    // (110:4) <Card style="z-index:1">
    function create_default_slot$2(ctx) {
    	let current;
    	const default_slot_template = /*#slots*/ ctx[18].default;
    	const default_slot = create_slot(default_slot_template, ctx, /*$$scope*/ ctx[20], null);

    	const block = {
    		c: function create() {
    			if (default_slot) default_slot.c();
    		},
    		m: function mount(target, anchor) {
    			if (default_slot) {
    				default_slot.m(target, anchor);
    			}

    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			if (default_slot) {
    				if (default_slot.p && (!current || dirty & /*$$scope*/ 1048576)) {
    					update_slot_base(
    						default_slot,
    						default_slot_template,
    						ctx,
    						/*$$scope*/ ctx[20],
    						!current
    						? get_all_dirty_from_scope(/*$$scope*/ ctx[20])
    						: get_slot_changes(default_slot_template, /*$$scope*/ ctx[20], dirty, null),
    						null
    					);
    				}
    			}
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(default_slot, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(default_slot, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (default_slot) default_slot.d(detaching);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_default_slot$2.name,
    		type: "slot",
    		source: "(110:4) <Card style=\\\"z-index:1\\\">",
    		ctx
    	});

    	return block;
    }

    // (83:0) {#if icon}
    function create_if_block_2(ctx) {
    	let span;
    	let icon_1;
    	let current;

    	icon_1 = new Icon({
    			props: { src: /*icon*/ ctx[9], size: "24px" },
    			$$inline: true
    		});

    	const block = {
    		c: function create() {
    			span = element("span");
    			create_component(icon_1.$$.fragment);
    			attr_dev(span, "class", "lefticon svelte-1o5ccdl");
    			add_location(span, file$2, 82, 11, 2036);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, span, anchor);
    			mount_component(icon_1, span, null);
    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			const icon_1_changes = {};
    			if (dirty & /*icon*/ 512) icon_1_changes.src = /*icon*/ ctx[9];
    			icon_1.$set(icon_1_changes);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(icon_1.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(icon_1.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(span);
    			destroy_component(icon_1);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block_2.name,
    		type: "if",
    		source: "(83:0) {#if icon}",
    		ctx
    	});

    	return block;
    }

    // (85:0) {#if iconRight}
    function create_if_block_1(ctx) {
    	let span;
    	let icon_1;
    	let current;

    	icon_1 = new Icon({
    			props: { src: /*iconRight*/ ctx[10], size: "24px" },
    			$$inline: true
    		});

    	const block = {
    		c: function create() {
    			span = element("span");
    			create_component(icon_1.$$.fragment);
    			attr_dev(span, "class", "righticon svelte-1o5ccdl");
    			add_location(span, file$2, 84, 16, 2134);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, span, anchor);
    			mount_component(icon_1, span, null);
    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			const icon_1_changes = {};
    			if (dirty & /*iconRight*/ 1024) icon_1_changes.src = /*iconRight*/ ctx[10];
    			icon_1.$set(icon_1_changes);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(icon_1.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(icon_1.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(span);
    			destroy_component(icon_1);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block_1.name,
    		type: "if",
    		source: "(85:0) {#if iconRight}",
    		ctx
    	});

    	return block;
    }

    function create_fragment$2(ctx) {
    	let current_block_type_index;
    	let if_block;
    	let if_block_anchor;
    	let current;
    	const if_block_creators = [create_if_block$1, create_else_block$1];
    	const if_blocks = [];

    	function select_block_type(ctx, dirty) {
    		if (/*dropdown*/ ctx[11] === false) return 0;
    		return 1;
    	}

    	current_block_type_index = select_block_type(ctx);
    	if_block = if_blocks[current_block_type_index] = if_block_creators[current_block_type_index](ctx);

    	const block = {
    		c: function create() {
    			if_block.c();
    			if_block_anchor = empty();
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			if_blocks[current_block_type_index].m(target, anchor);
    			insert_dev(target, if_block_anchor, anchor);
    			current = true;
    		},
    		p: function update(ctx, [dirty]) {
    			let previous_block_index = current_block_type_index;
    			current_block_type_index = select_block_type(ctx);

    			if (current_block_type_index === previous_block_index) {
    				if_blocks[current_block_type_index].p(ctx, dirty);
    			} else {
    				group_outros();

    				transition_out(if_blocks[previous_block_index], 1, 1, () => {
    					if_blocks[previous_block_index] = null;
    				});

    				check_outros();
    				if_block = if_blocks[current_block_type_index];

    				if (!if_block) {
    					if_block = if_blocks[current_block_type_index] = if_block_creators[current_block_type_index](ctx);
    					if_block.c();
    				} else {
    					if_block.p(ctx, dirty);
    				}

    				transition_in(if_block, 1);
    				if_block.m(if_block_anchor.parentNode, if_block_anchor);
    			}
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(if_block);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(if_block);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if_blocks[current_block_type_index].d(detaching);
    			if (detaching) detach_dev(if_block_anchor);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$2.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function dropdownAction(node, param) {
    	let autoclose = param;
    	let button = node.getElementsByTagName('summary')[0];

    	const clickOutside = () => {
    		if (!!node.open) node.open = false;
    	};

    	const clickButton = e => {
    		e.stopPropagation();
    	};

    	const clickInDD = e => {
    		e.stopPropagation();
    		if (autoclose) node.open = false;
    	};

    	node.addEventListener('click', clickInDD);
    	button.addEventListener('click', clickButton);
    	window.addEventListener('click', clickOutside);

    	return {
    		update: param => autoclose = param,
    		destroy: () => {
    			window.removeEventListener('click', clickOutside);
    			node.removeEventListener('click', clickInDD);
    			button.removeEventListener('click', clickButton);
    		}
    	};
    }

    function instance$2($$self, $$props, $$invalidate) {
    	let clIcon;
    	let clIcononly;

    	const omit_props_names = [
    		"outline","primary","secondary","dark","error","success","clear","loading","icon","iconRight","dropdown","open","autoclose","submit"
    	];

    	let $$restProps = compute_rest_props($$props, omit_props_names);
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('Button', slots, ['default']);
    	let { outline = null } = $$props;
    	let { primary = null } = $$props;
    	let { secondary = null } = $$props;
    	let { dark = null } = $$props;
    	let { error = null } = $$props;
    	let { success = null } = $$props;
    	let { clear = null } = $$props;
    	let { loading = null } = $$props;
    	let { icon = null } = $$props;
    	let { iconRight = null } = $$props;
    	let { dropdown = false } = $$props;
    	let { open = false } = $$props;
    	let { autoclose = false } = $$props;
    	let { submit = false } = $$props;
    	const events = getEventsAction();
    	const hasSlot = $$props.$$slots && $$props.$$slots !== undefined;

    	function details_toggle_handler() {
    		open = this.open;
    		$$invalidate(0, open);
    	}

    	$$self.$$set = $$new_props => {
    		$$invalidate(22, $$props = assign(assign({}, $$props), exclude_internal_props($$new_props)));
    		$$invalidate(17, $$restProps = compute_rest_props($$props, omit_props_names));
    		if ('outline' in $$new_props) $$invalidate(1, outline = $$new_props.outline);
    		if ('primary' in $$new_props) $$invalidate(2, primary = $$new_props.primary);
    		if ('secondary' in $$new_props) $$invalidate(3, secondary = $$new_props.secondary);
    		if ('dark' in $$new_props) $$invalidate(4, dark = $$new_props.dark);
    		if ('error' in $$new_props) $$invalidate(5, error = $$new_props.error);
    		if ('success' in $$new_props) $$invalidate(6, success = $$new_props.success);
    		if ('clear' in $$new_props) $$invalidate(7, clear = $$new_props.clear);
    		if ('loading' in $$new_props) $$invalidate(8, loading = $$new_props.loading);
    		if ('icon' in $$new_props) $$invalidate(9, icon = $$new_props.icon);
    		if ('iconRight' in $$new_props) $$invalidate(10, iconRight = $$new_props.iconRight);
    		if ('dropdown' in $$new_props) $$invalidate(11, dropdown = $$new_props.dropdown);
    		if ('open' in $$new_props) $$invalidate(0, open = $$new_props.open);
    		if ('autoclose' in $$new_props) $$invalidate(12, autoclose = $$new_props.autoclose);
    		if ('submit' in $$new_props) $$invalidate(13, submit = $$new_props.submit);
    		if ('$$scope' in $$new_props) $$invalidate(20, $$scope = $$new_props.$$scope);
    	};

    	$$self.$capture_state = () => ({
    		getEventsAction,
    		Card,
    		Icon,
    		outline,
    		primary,
    		secondary,
    		dark,
    		error,
    		success,
    		clear,
    		loading,
    		icon,
    		iconRight,
    		dropdown,
    		open,
    		autoclose,
    		submit,
    		events,
    		hasSlot,
    		dropdownAction,
    		clIcononly,
    		clIcon
    	});

    	$$self.$inject_state = $$new_props => {
    		$$invalidate(22, $$props = assign(assign({}, $$props), $$new_props));
    		if ('outline' in $$props) $$invalidate(1, outline = $$new_props.outline);
    		if ('primary' in $$props) $$invalidate(2, primary = $$new_props.primary);
    		if ('secondary' in $$props) $$invalidate(3, secondary = $$new_props.secondary);
    		if ('dark' in $$props) $$invalidate(4, dark = $$new_props.dark);
    		if ('error' in $$props) $$invalidate(5, error = $$new_props.error);
    		if ('success' in $$props) $$invalidate(6, success = $$new_props.success);
    		if ('clear' in $$props) $$invalidate(7, clear = $$new_props.clear);
    		if ('loading' in $$props) $$invalidate(8, loading = $$new_props.loading);
    		if ('icon' in $$props) $$invalidate(9, icon = $$new_props.icon);
    		if ('iconRight' in $$props) $$invalidate(10, iconRight = $$new_props.iconRight);
    		if ('dropdown' in $$props) $$invalidate(11, dropdown = $$new_props.dropdown);
    		if ('open' in $$props) $$invalidate(0, open = $$new_props.open);
    		if ('autoclose' in $$props) $$invalidate(12, autoclose = $$new_props.autoclose);
    		if ('submit' in $$props) $$invalidate(13, submit = $$new_props.submit);
    		if ('clIcononly' in $$props) $$invalidate(14, clIcononly = $$new_props.clIcononly);
    		if ('clIcon' in $$props) $$invalidate(15, clIcon = $$new_props.clIcon);
    	};

    	if ($$props && "$$inject" in $$props) {
    		$$self.$inject_state($$props.$$inject);
    	}

    	$$self.$$.update = () => {
    		if ($$self.$$.dirty & /*icon, iconRight*/ 1536) {
    			$$invalidate(15, clIcon = (icon !== null || iconRight !== null) && hasSlot);
    		}

    		if ($$self.$$.dirty & /*dropdown, icon*/ 2560) {
    			$$invalidate(14, clIcononly = dropdown
    			? icon !== null && dropdown === true
    			: icon !== null && !hasSlot);
    		}
    	};

    	$$props = exclude_internal_props($$props);

    	return [
    		open,
    		outline,
    		primary,
    		secondary,
    		dark,
    		error,
    		success,
    		clear,
    		loading,
    		icon,
    		iconRight,
    		dropdown,
    		autoclose,
    		submit,
    		clIcononly,
    		clIcon,
    		events,
    		$$restProps,
    		slots,
    		details_toggle_handler,
    		$$scope
    	];
    }

    class Button extends SvelteComponentDev {
    	constructor(options) {
    		super(options);

    		init(this, options, instance$2, create_fragment$2, safe_not_equal, {
    			outline: 1,
    			primary: 2,
    			secondary: 3,
    			dark: 4,
    			error: 5,
    			success: 6,
    			clear: 7,
    			loading: 8,
    			icon: 9,
    			iconRight: 10,
    			dropdown: 11,
    			open: 0,
    			autoclose: 12,
    			submit: 13
    		});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "Button",
    			options,
    			id: create_fragment$2.name
    		});
    	}

    	get outline() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set outline(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get primary() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set primary(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get secondary() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set secondary(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get dark() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set dark(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get error() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set error(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get success() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set success(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get clear() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set clear(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get loading() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set loading(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get icon() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set icon(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get iconRight() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set iconRight(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get dropdown() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set dropdown(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get open() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set open(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get autoclose() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set autoclose(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	get submit() {
    		throw new Error("<Button>: Props cannot be read directly from the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}

    	set submit(value) {
    		throw new Error("<Button>: Props cannot be set directly on the component instance unless compiling with 'accessors: true' or '<svelte:options accessors/>'");
    	}
    }

    /* src\components\LocaleSelector.svelte generated by Svelte v3.59.2 */
    const file$1 = "src\\components\\LocaleSelector.svelte";

    // (12:0) <Button dropdown={$localeDescription} autoclose outline>
    function create_default_slot$1(ctx) {
    	let p0;
    	let a0;
    	let t1;
    	let p1;
    	let a1;
    	let t3;
    	let p2;
    	let a2;
    	let t5;
    	let p3;
    	let a3;
    	let mounted;
    	let dispose;

    	const block = {
    		c: function create() {
    			p0 = element("p");
    			a0 = element("a");
    			a0.textContent = "Deutsch";
    			t1 = space();
    			p1 = element("p");
    			a1 = element("a");
    			a1.textContent = "English";
    			t3 = space();
    			p2 = element("p");
    			a2 = element("a");
    			a2.textContent = "Nederlands";
    			t5 = space();
    			p3 = element("p");
    			a3 = element("a");
    			a3.textContent = "Srpski";
    			attr_dev(a0, "href", null);
    			add_location(a0, file$1, 12, 7, 361);
    			add_location(p0, file$1, 12, 4, 358);
    			attr_dev(a1, "href", null);
    			add_location(a1, file$1, 13, 7, 456);
    			add_location(p1, file$1, 13, 4, 453);
    			attr_dev(a2, "href", null);
    			add_location(a2, file$1, 14, 7, 551);
    			add_location(p2, file$1, 14, 4, 548);
    			attr_dev(a3, "href", null);
    			add_location(a3, file$1, 15, 7, 649);
    			add_location(p3, file$1, 15, 4, 646);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, p0, anchor);
    			append_dev(p0, a0);
    			insert_dev(target, t1, anchor);
    			insert_dev(target, p1, anchor);
    			append_dev(p1, a1);
    			insert_dev(target, t3, anchor);
    			insert_dev(target, p2, anchor);
    			append_dev(p2, a2);
    			insert_dev(target, t5, anchor);
    			insert_dev(target, p3, anchor);
    			append_dev(p3, a3);

    			if (!mounted) {
    				dispose = [
    					listen_dev(a0, "click", prevent_default(/*click_handler*/ ctx[2]), false, true, false, false),
    					listen_dev(a1, "click", prevent_default(/*click_handler_1*/ ctx[3]), false, true, false, false),
    					listen_dev(a2, "click", prevent_default(/*click_handler_2*/ ctx[4]), false, true, false, false),
    					listen_dev(a3, "click", prevent_default(/*click_handler_3*/ ctx[5]), false, true, false, false)
    				];

    				mounted = true;
    			}
    		},
    		p: noop,
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(p0);
    			if (detaching) detach_dev(t1);
    			if (detaching) detach_dev(p1);
    			if (detaching) detach_dev(t3);
    			if (detaching) detach_dev(p2);
    			if (detaching) detach_dev(t5);
    			if (detaching) detach_dev(p3);
    			mounted = false;
    			run_all(dispose);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_default_slot$1.name,
    		type: "slot",
    		source: "(12:0) <Button dropdown={$localeDescription} autoclose outline>",
    		ctx
    	});

    	return block;
    }

    function create_fragment$1(ctx) {
    	let button;
    	let current;

    	button = new Button({
    			props: {
    				dropdown: /*$localeDescription*/ ctx[0],
    				autoclose: true,
    				outline: true,
    				$$slots: { default: [create_default_slot$1] },
    				$$scope: { ctx }
    			},
    			$$inline: true
    		});

    	const block = {
    		c: function create() {
    			create_component(button.$$.fragment);
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			mount_component(button, target, anchor);
    			current = true;
    		},
    		p: function update(ctx, [dirty]) {
    			const button_changes = {};
    			if (dirty & /*$localeDescription*/ 1) button_changes.dropdown = /*$localeDescription*/ ctx[0];

    			if (dirty & /*$$scope*/ 128) {
    				button_changes.$$scope = { dirty, ctx };
    			}

    			button.$set(button_changes);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(button.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(button.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			destroy_component(button, detaching);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment$1.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    function instance$1($$self, $$props, $$invalidate) {
    	let $localeDescription;
    	validate_store(localeDescription, 'localeDescription');
    	component_subscribe($$self, localeDescription, $$value => $$invalidate(0, $localeDescription = $$value));
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('LocaleSelector', slots, []);
    	const dispatch = createEventDispatcher();

    	const handleLocaleChange = lang => {
    		dispatch("locale-changed", lang);
    	};

    	const writable_props = [];

    	Object.keys($$props).forEach(key => {
    		if (!~writable_props.indexOf(key) && key.slice(0, 2) !== '$$' && key !== 'slot') console.warn(`<LocaleSelector> was created with unknown prop '${key}'`);
    	});

    	const click_handler = () => handleLocaleChange("de");
    	const click_handler_1 = () => handleLocaleChange("en");
    	const click_handler_2 = () => handleLocaleChange("nl");
    	const click_handler_3 = () => handleLocaleChange("rs");

    	$$self.$capture_state = () => ({
    		createEventDispatcher,
    		localeDescription,
    		Button,
    		dispatch,
    		handleLocaleChange,
    		$localeDescription
    	});

    	return [
    		$localeDescription,
    		handleLocaleChange,
    		click_handler,
    		click_handler_1,
    		click_handler_2,
    		click_handler_3
    	];
    }

    class LocaleSelector extends SvelteComponentDev {
    	constructor(options) {
    		super(options);
    		init(this, options, instance$1, create_fragment$1, safe_not_equal, {});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "LocaleSelector",
    			options,
    			id: create_fragment$1.name
    		});
    	}
    }

    /* src\App.svelte generated by Svelte v3.59.2 */
    const file = "src\\App.svelte";

    // (38:16) {:else}
    function create_else_block(ctx) {
    	let a0;
    	let t0_value = /*$_*/ ctx[1]("workouts") + "";
    	let t0;
    	let t1;
    	let a1;
    	let t2_value = /*$_*/ ctx[1]("logout") + "";
    	let t2;
    	let mounted;
    	let dispose;

    	const block = {
    		c: function create() {
    			a0 = element("a");
    			t0 = text(t0_value);
    			t1 = space();
    			a1 = element("a");
    			t2 = text(t2_value);
    			attr_dev(a0, "href", "/workouts");
    			toggle_class(a0, "active", /*$location*/ ctx[0] === "/workouts");
    			add_location(a0, file, 38, 20, 1292);
    			attr_dev(a1, "href", "/logout");
    			toggle_class(a1, "active", /*$location*/ ctx[0] === "/logout");
    			add_location(a1, file, 43, 20, 1496);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, a0, anchor);
    			append_dev(a0, t0);
    			insert_dev(target, t1, anchor);
    			insert_dev(target, a1, anchor);
    			append_dev(a1, t2);

    			if (!mounted) {
    				dispose = [
    					action_destroyer(link.call(null, a0)),
    					action_destroyer(link.call(null, a1))
    				];

    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*$_*/ 2 && t0_value !== (t0_value = /*$_*/ ctx[1]("workouts") + "")) set_data_dev(t0, t0_value);

    			if (dirty & /*$location*/ 1) {
    				toggle_class(a0, "active", /*$location*/ ctx[0] === "/workouts");
    			}

    			if (dirty & /*$_*/ 2 && t2_value !== (t2_value = /*$_*/ ctx[1]("logout") + "")) set_data_dev(t2, t2_value);

    			if (dirty & /*$location*/ 1) {
    				toggle_class(a1, "active", /*$location*/ ctx[0] === "/logout");
    			}
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(a0);
    			if (detaching) detach_dev(t1);
    			if (detaching) detach_dev(a1);
    			mounted = false;
    			run_all(dispose);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_else_block.name,
    		type: "else",
    		source: "(38:16) {:else}",
    		ctx
    	});

    	return block;
    }

    // (32:16) {#if !$s.loggedIn}
    function create_if_block(ctx) {
    	let a;
    	let t_value = /*$_*/ ctx[1]("login") + "";
    	let t;
    	let mounted;
    	let dispose;

    	const block = {
    		c: function create() {
    			a = element("a");
    			t = text(t_value);
    			attr_dev(a, "href", "/login");
    			toggle_class(a, "active", /*$location*/ ctx[0] === "/login");
    			add_location(a, file, 32, 20, 1073);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, a, anchor);
    			append_dev(a, t);

    			if (!mounted) {
    				dispose = action_destroyer(link.call(null, a));
    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if (dirty & /*$_*/ 2 && t_value !== (t_value = /*$_*/ ctx[1]("login") + "")) set_data_dev(t, t_value);

    			if (dirty & /*$location*/ 1) {
    				toggle_class(a, "active", /*$location*/ ctx[0] === "/login");
    			}
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(a);
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_if_block.name,
    		type: "if",
    		source: "(32:16) {#if !$s.loggedIn}",
    		ctx
    	});

    	return block;
    }

    // (55:8) <Col size="8" sizeMD="8" sizeLG="8">
    function create_default_slot_3(ctx) {
    	let main;
    	let router;
    	let current;

    	router = new Router({
    			props: {
    				routes: {
    					"/": wrap$1({ component: Home, conditions: [func] }),
    					"/workouts": wrap$1({
    						component: Workouts,
    						conditions: [/*func_1*/ ctx[4]]
    					}),
    					"/login": wrap$1({
    						component: Login,
    						conditions: [/*func_2*/ ctx[5]]
    					}),
    					"/logout": wrap$1({
    						component: Logout,
    						conditions: [/*func_3*/ ctx[6]]
    					})
    				}
    			},
    			$$inline: true
    		});

    	const block = {
    		c: function create() {
    			main = element("main");
    			create_component(router.$$.fragment);
    			add_location(main, file, 55, 12, 1809);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, main, anchor);
    			mount_component(router, main, null);
    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			const router_changes = {};

    			if (dirty & /*$s*/ 4) router_changes.routes = {
    				"/": wrap$1({ component: Home, conditions: [func] }),
    				"/workouts": wrap$1({
    					component: Workouts,
    					conditions: [/*func_1*/ ctx[4]]
    				}),
    				"/login": wrap$1({
    					component: Login,
    					conditions: [/*func_2*/ ctx[5]]
    				}),
    				"/logout": wrap$1({
    					component: Logout,
    					conditions: [/*func_3*/ ctx[6]]
    				})
    			};

    			router.$set(router_changes);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(router.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(router.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(main);
    			destroy_component(router);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_default_slot_3.name,
    		type: "slot",
    		source: "(55:8) <Col size=\\\"8\\\" sizeMD=\\\"8\\\" sizeLG=\\\"8\\\">",
    		ctx
    	});

    	return block;
    }

    // (107:8) <Col size="4" sizeMD="4" sizeLG="4">
    function create_default_slot_2(ctx) {
    	let aside;
    	let div;
    	let localeselector;
    	let current;
    	localeselector = new LocaleSelector({ $$inline: true });
    	localeselector.$on("locale-changed", /*locale_changed_handler*/ ctx[7]);

    	const block = {
    		c: function create() {
    			aside = element("aside");
    			div = element("div");
    			create_component(localeselector.$$.fragment);
    			attr_dev(div, "class", "nav-right langselect svelte-1cc8y8b");
    			add_location(div, file, 108, 16, 3884);
    			add_location(aside, file, 107, 12, 3860);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, aside, anchor);
    			append_dev(aside, div);
    			mount_component(localeselector, div, null);
    			current = true;
    		},
    		p: noop,
    		i: function intro(local) {
    			if (current) return;
    			transition_in(localeselector.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(localeselector.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(aside);
    			destroy_component(localeselector);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_default_slot_2.name,
    		type: "slot",
    		source: "(107:8) <Col size=\\\"4\\\" sizeMD=\\\"4\\\" sizeLG=\\\"4\\\">",
    		ctx
    	});

    	return block;
    }

    // (54:4) <Row>
    function create_default_slot_1(ctx) {
    	let col0;
    	let t;
    	let col1;
    	let current;

    	col0 = new Col({
    			props: {
    				size: "8",
    				sizeMD: "8",
    				sizeLG: "8",
    				$$slots: { default: [create_default_slot_3] },
    				$$scope: { ctx }
    			},
    			$$inline: true
    		});

    	col1 = new Col({
    			props: {
    				size: "4",
    				sizeMD: "4",
    				sizeLG: "4",
    				$$slots: { default: [create_default_slot_2] },
    				$$scope: { ctx }
    			},
    			$$inline: true
    		});

    	const block = {
    		c: function create() {
    			create_component(col0.$$.fragment);
    			t = space();
    			create_component(col1.$$.fragment);
    		},
    		m: function mount(target, anchor) {
    			mount_component(col0, target, anchor);
    			insert_dev(target, t, anchor);
    			mount_component(col1, target, anchor);
    			current = true;
    		},
    		p: function update(ctx, dirty) {
    			const col0_changes = {};

    			if (dirty & /*$$scope, $s*/ 516) {
    				col0_changes.$$scope = { dirty, ctx };
    			}

    			col0.$set(col0_changes);
    			const col1_changes = {};

    			if (dirty & /*$$scope*/ 512) {
    				col1_changes.$$scope = { dirty, ctx };
    			}

    			col1.$set(col1_changes);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(col0.$$.fragment, local);
    			transition_in(col1.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(col0.$$.fragment, local);
    			transition_out(col1.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			destroy_component(col0, detaching);
    			if (detaching) detach_dev(t);
    			destroy_component(col1, detaching);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_default_slot_1.name,
    		type: "slot",
    		source: "(54:4) <Row>",
    		ctx
    	});

    	return block;
    }

    // (25:0) <Container>
    function create_default_slot(ctx) {
    	let nav;
    	let div1;
    	let div0;
    	let a;
    	let t0_value = /*$_*/ ctx[1]("home") + "";
    	let t0;
    	let t1;
    	let t2;
    	let row;
    	let t3;
    	let footer;
    	let current;
    	let mounted;
    	let dispose;

    	function select_block_type(ctx, dirty) {
    		if (!/*$s*/ ctx[2].loggedIn) return create_if_block;
    		return create_else_block;
    	}

    	let current_block_type = select_block_type(ctx);
    	let if_block = current_block_type(ctx);

    	row = new Row({
    			props: {
    				$$slots: { default: [create_default_slot_1] },
    				$$scope: { ctx }
    			},
    			$$inline: true
    		});

    	const block = {
    		c: function create() {
    			nav = element("nav");
    			div1 = element("div");
    			div0 = element("div");
    			a = element("a");
    			t0 = text(t0_value);
    			t1 = space();
    			if_block.c();
    			t2 = space();
    			create_component(row.$$.fragment);
    			t3 = space();
    			footer = element("footer");
    			footer.textContent = "(C) 2023 by Keza";
    			attr_dev(a, "href", "/");
    			toggle_class(a, "active", /*$location*/ ctx[0] === "/");
    			add_location(a, file, 28, 16, 909);
    			attr_dev(div0, "class", "tabs");
    			add_location(div0, file, 27, 12, 874);
    			attr_dev(div1, "class", "nav-left");
    			add_location(div1, file, 26, 8, 839);
    			attr_dev(nav, "class", "nav");
    			add_location(nav, file, 25, 4, 813);
    			add_location(footer, file, 118, 4, 4168);
    		},
    		m: function mount(target, anchor) {
    			insert_dev(target, nav, anchor);
    			append_dev(nav, div1);
    			append_dev(div1, div0);
    			append_dev(div0, a);
    			append_dev(a, t0);
    			append_dev(div0, t1);
    			if_block.m(div0, null);
    			insert_dev(target, t2, anchor);
    			mount_component(row, target, anchor);
    			insert_dev(target, t3, anchor);
    			insert_dev(target, footer, anchor);
    			current = true;

    			if (!mounted) {
    				dispose = action_destroyer(link.call(null, a));
    				mounted = true;
    			}
    		},
    		p: function update(ctx, dirty) {
    			if ((!current || dirty & /*$_*/ 2) && t0_value !== (t0_value = /*$_*/ ctx[1]("home") + "")) set_data_dev(t0, t0_value);

    			if (!current || dirty & /*$location*/ 1) {
    				toggle_class(a, "active", /*$location*/ ctx[0] === "/");
    			}

    			if (current_block_type === (current_block_type = select_block_type(ctx)) && if_block) {
    				if_block.p(ctx, dirty);
    			} else {
    				if_block.d(1);
    				if_block = current_block_type(ctx);

    				if (if_block) {
    					if_block.c();
    					if_block.m(div0, null);
    				}
    			}

    			const row_changes = {};

    			if (dirty & /*$$scope, $s*/ 516) {
    				row_changes.$$scope = { dirty, ctx };
    			}

    			row.$set(row_changes);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(row.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(row.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			if (detaching) detach_dev(nav);
    			if_block.d();
    			if (detaching) detach_dev(t2);
    			destroy_component(row, detaching);
    			if (detaching) detach_dev(t3);
    			if (detaching) detach_dev(footer);
    			mounted = false;
    			dispose();
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_default_slot.name,
    		type: "slot",
    		source: "(25:0) <Container>",
    		ctx
    	});

    	return block;
    }

    function create_fragment(ctx) {
    	let container;
    	let current;

    	container = new Container({
    			props: {
    				$$slots: { default: [create_default_slot] },
    				$$scope: { ctx }
    			},
    			$$inline: true
    		});

    	const block = {
    		c: function create() {
    			create_component(container.$$.fragment);
    		},
    		l: function claim(nodes) {
    			throw new Error("options.hydrate only works if the component was compiled with the `hydratable: true` option");
    		},
    		m: function mount(target, anchor) {
    			mount_component(container, target, anchor);
    			current = true;
    		},
    		p: function update(ctx, [dirty]) {
    			const container_changes = {};

    			if (dirty & /*$$scope, $s, $location, $_*/ 519) {
    				container_changes.$$scope = { dirty, ctx };
    			}

    			container.$set(container_changes);
    		},
    		i: function intro(local) {
    			if (current) return;
    			transition_in(container.$$.fragment, local);
    			current = true;
    		},
    		o: function outro(local) {
    			transition_out(container.$$.fragment, local);
    			current = false;
    		},
    		d: function destroy(detaching) {
    			destroy_component(container, detaching);
    		}
    	};

    	dispatch_dev("SvelteRegisterBlock", {
    		block,
    		id: create_fragment.name,
    		type: "component",
    		source: "",
    		ctx
    	});

    	return block;
    }

    const func = () => {
    	return true;
    };

    function instance($$self, $$props, $$invalidate) {
    	let $locale$1;
    	let $isLocaleLoaded;
    	let $location;
    	let $_;
    	let $s;
    	validate_store($locale, 'locale');
    	component_subscribe($$self, $locale, $$value => $$invalidate(8, $locale$1 = $$value));
    	validate_store(isLocaleLoaded, 'isLocaleLoaded');
    	component_subscribe($$self, isLocaleLoaded, $$value => $$invalidate(3, $isLocaleLoaded = $$value));
    	validate_store(location, 'location');
    	component_subscribe($$self, location, $$value => $$invalidate(0, $location = $$value));
    	validate_store($format, '_');
    	component_subscribe($$self, $format, $$value => $$invalidate(1, $_ = $$value));
    	validate_store(s, 's');
    	component_subscribe($$self, s, $$value => $$invalidate(2, $s = $$value));
    	let { $$slots: slots = {}, $$scope } = $$props;
    	validate_slots('App', slots, []);
    	const writable_props = [];

    	Object.keys($$props).forEach(key => {
    		if (!~writable_props.indexOf(key) && key.slice(0, 2) !== '$$' && key !== 'slot') console.warn(`<App> was created with unknown prop '${key}'`);
    	});

    	const func_1 = () => {
    		if (!$s.loggedIn) {
    			push("/login");
    			return false;
    		}

    		return true;
    	};

    	const func_2 = () => {
    		if ($s.loggedIn) {
    			push("/");
    			return false;
    		}

    		return true;
    	};

    	const func_3 = () => {
    		if (!$s.loggedIn) {
    			push("/login");
    			return false;
    		}

    		return true;
    	};

    	const locale_changed_handler = e => setupI18n({ withLocale: e.detail });

    	$$self.$capture_state = () => ({
    		s,
    		Router,
    		wrap: wrap$1,
    		push,
    		link,
    		location,
    		setupI18n,
    		isLocaleLoaded,
    		locale: $locale,
    		_: $format,
    		Home,
    		Workouts,
    		Login,
    		Logout,
    		LocaleSelector,
    		Container,
    		Row,
    		Col,
    		$locale: $locale$1,
    		$isLocaleLoaded,
    		$location,
    		$_,
    		$s
    	});

    	$$self.$$.update = () => {
    		if ($$self.$$.dirty & /*$isLocaleLoaded*/ 8) {
    			{
    				if (!$isLocaleLoaded) {
    					set_store_value($locale, $locale$1 = "rs", $locale$1);
    					setupI18n({ withLocale: "rs" });
    				}
    			}
    		}
    	};

    	return [
    		$location,
    		$_,
    		$s,
    		$isLocaleLoaded,
    		func_1,
    		func_2,
    		func_3,
    		locale_changed_handler
    	];
    }

    class App extends SvelteComponentDev {
    	constructor(options) {
    		super(options);
    		init(this, options, instance, create_fragment, safe_not_equal, {});

    		dispatch_dev("SvelteRegisterComponent", {
    			component: this,
    			tagName: "App",
    			options,
    			id: create_fragment.name
    		});
    	}
    }

    const app = new App({
        target: document.body
    });

    return app;

})();
//# sourceMappingURL=bundle.js.map
