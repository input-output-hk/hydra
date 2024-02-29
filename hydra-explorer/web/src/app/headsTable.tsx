"use client"; // This is a client component 👈🏽

import { useState, useEffect } from "react"
import Image from "next/image"

interface HeadState {
    headId: string
    status: string
}

const HeadsTable = () => {
    const [Heads, setHeads] = useState<HeadState[]>([])
    const [error, setError] = useState<string | null>(null)

    useEffect(() => {
        getHeads()
    }, [])

    const getHeads = async () => {
        try {
            const response = await fetch('http://explorer.hydra.family/heads')
            // The return value is *not* serialized
            // You can return Date, Map, Set, etc.
            if (!response.ok) {
                // This will activate the closest `error.js` Error Boundary
                throw new Error('Failed to fetch data')
            }
            const data: HeadState[] = await response.json()
            setHeads(data)
        } catch (error) {
            setError('Error fetching data. Please try again later.')
        }
    }

    return (
        <div className="container mx-auto mt-8">
            <h1 className="text-3xl font-bold mb-4 flex items-center">
                <div className="mr-2">
                    <Image
                        src="/hydra.svg"
                        alt="Hydra Logo"
                        className="dark:invert"
                        width={100}
                        height={24}
                        priority
                    />
                </div>
                Hydrascan
            </h1>
            {error ? (
                <p className="text-red-500">{error}</p>
            ) : (
                <div className="w-full">
                    <table className="table-fixed w-full bg-gray-800 text-white rounded-lg">
                        <thead>
                            <tr>
                                <th className="px-4 py-2">Head ID</th>
                                <th className="px-4 py-2">Status</th>
                            </tr>
                        </thead>
                        <tbody>
                            {Heads.map((entry, index) => (
                                <tr key={index} className={`${index % 2 === 0 ? 'bg-gray-700' : 'bg-gray-600'}`}>
                                    <td className="border px-4 py-2">{entry.headId}</td>
                                    <td className="border px-4 py-2">{entry.status}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
        </div>
    )
}

export default HeadsTable