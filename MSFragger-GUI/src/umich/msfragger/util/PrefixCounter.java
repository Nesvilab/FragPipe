/*
 * Copyright 2018 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package umich.msfragger.util;

import java.util.ArrayDeque;
import java.util.Map;
import java.util.TreeMap;

/**
 * A trie like structure that counts the prefixes or suffixes in a corpus of words.
 * Used to determine common 
 * @author Dmitry Avtonomov
 */
public class PrefixCounter {
    
    public enum Mode {FWD, REV};
    public enum IterationOrder {BREADTH, DEPTH}
    
    protected Node root;
    public final Mode mode;
    public final int maxDepth;
    
    public PrefixCounter(Mode mode, int maxDepth) {
        this.mode = mode;
        this.maxDepth = maxDepth;
        this.root = new Node(null, '\uFFFF', 0); // \uFFFF is 'not a character'
    }
    
    public class Node {
        public final Node parent;
        private final TreeMap<Character, Node> map;
        public final char ch;
        public final int depth;
        private long hits;
        private long terminals;

        protected Node(Node parent, char ch, int depth) {
            this.parent = parent;
            this.ch = ch;
            this.depth = depth;
            map = new TreeMap<>();
            hits = 0;
            terminals = 0;
        }
        
        protected Node getChild(char ch) {
            Node child = map.get(ch);
            if (child == null) {
                child = new Node(this, ch, this.depth + 1);
                map.put(child.ch, child);
            }
            return child;
        }
        
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("Node{");
            sb.append(" parent_char=");
            if (parent != null)
                sb.append(parent.ch);
            else 
                sb.append("None");
            sb.append(", char=").append(ch).append(", depth=").append(depth)
              .append(", size=").append(map.size()).append(", hits").append(hits)
              .append(", terminals=").append(terminals);
            sb.append('}');
            return sb.toString();
        }
        
        public long getHits() {
            return hits;
        }
        
        public long getTerminals() {
            return terminals;
        }
    }

    public Node getRoot() {
        return root;
    }

    public Mode getMode() {
        return mode;
    }
    
    public void add(CharSequence csq) {
        switch (mode) {
            case FWD: {
                int pos = -1;
                int len = csq.length();
                Node n = root;
                n.hits++;
                if (len == 0)
                    n.terminals++;
                while (++pos < len) {
                    Node nn = n.getChild(csq.charAt(pos));
                    if (nn.depth > maxDepth)
                        return;
                    nn.hits++;
                    if (pos == len - 1)
                        nn.terminals++;
                    
                    n = nn;
                }
                break;
            }
            
            case REV: {
                int pos = csq.length();
                Node n = root;
                n.hits++;
                while (--pos >= 0) {
                    Node nn = n.getChild(csq.charAt(pos));
                    if (nn.depth > maxDepth)
                        return;
                    nn.hits++;
                    n = nn;
                    if (pos == 0)
                        nn.terminals++;
                }
                break;
            }
        }
    }
    
    public void iterPrefixCounts(int maxDepth, Proc2<Node, Mode> action) {
        
        final ArrayDeque<Node> deque = new ArrayDeque<>();
        deque.add(root);
        while (!deque.isEmpty()) {
            Node head = deque.removeFirst();
            if (head.depth > maxDepth)
                break;
            for (Map.Entry<Character, Node> e : head.map.entrySet()) {
                deque.addLast(e.getValue());
            }
            action.call(head, mode);
        }
    }
    
    public void printPrefixCounts(int maxDepth) {
        ArrayDeque<Node> deque = new ArrayDeque<>();
        deque.add(root);
        while (!deque.isEmpty()) {
            Node head = deque.removeFirst();
            if (head.depth > maxDepth)
                break;
            for (Map.Entry<Character, Node> e : head.map.entrySet()) {
                deque.addLast(e.getValue());
            }
            System.out.println(head);
        }
    }
}
